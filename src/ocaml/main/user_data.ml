module C = Sxfiler_common
module M = Modules
open Sexplib.Std

type t = {
  left_pane_history: C.Pane_history.History.t list;
  right_pane_history: C.Pane_history.History.t list;
}
[@@deriving sexp]

let empty = {
  left_pane_history = [];
  right_pane_history = [];
}

let default_path () =
  let electron = M.electron () in
  Js.to_string @@ electron##.app##getPath (Js.string "userData")

let history_path () =
  let module N = Jsoo_node in
  N.Path.join [default_path (); "history"]

(** Load user data from [path]. If [path] does not specified, use default path defined in [default_path]. *)
let load ?path () =
  let history_path = match path with
    | Some path -> path
    | None -> history_path () in
  let open Minimal_monadic_caml.Result in
  let open Minimal_monadic_caml.Result.Infix in
  let module N = Jsoo_node in
  let loaded = N.Fs.readFileSync history_path
    >>= lift Sexplib.Sexp.of_string
    >>= lift t_of_sexp
  in
  match loaded with
  | Ok history -> history
  | Error error -> begin
      match error with
      | `JsooSystemError e -> begin
          match N.Errors.System_error.to_code e with
          | N.Errors.System_error.ENOENT -> empty
          | _ -> raise Errors.(to_error (`Sxfiler_node_error error))
        end
    end

let of_state state =
  let module S = C.State in
  let module PH = C.Pane_history in
  {
    left_pane_history = PH.sorted_history state.S.left_pane_history |> Array.to_list;
    right_pane_history = PH.sorted_history state.S.right_pane_history |> Array.to_list;
  }

(** Store user data [data] to [path]. If [path] does not specified, use default path
    defined in [default_path].
*)
let save ?path data =
  let history_path = match path with
    | Some path -> path
    | None -> history_path () in

  let open Minimal_monadic_caml.Result in
  let open Infix in
  let module N = Jsoo_node in
  let data = sexp_of_t data |> Sexplib.Sexp.to_string in
  match N.Fs.writeFileSync history_path data with
  | Ok () -> ()
  | Error e -> raise Errors.(to_error (`Sxfiler_node_error e))

module Test = struct
  open Mocha_of_ocaml
  module Fs = Jsoo_node.Fs

  let path = "tmp.config.d"
  let suite () =
    "User data" >::: [
      before_each (fun () -> Fs.mkdirSync path |> ignore);
      after_each (fun () -> Fs.remove_sync path |> ignore);
      "should be able to save and load data" >:: (fun () ->
          let module P = C.Pane_history.History in
          let module T = C.Types.File_id in
          let data = {
            left_pane_history = [P.make ~directory:"test" ~focused_item:T.empty ()];
            right_pane_history = [P.make ~directory:"tmp" ~focused_item:(T.make "tmp/file") ()];
          } in

          let path = Jsoo_node.Path.(join [path;"data"]) in
          save ~path data;
          let loaded = load ~path () in
          assert_ok (data = loaded)
        );
      "should return empty data if file not found" >:: (fun () ->
          let module P = C.Pane_history.History in
          let module T = C.Types.File_id in

          let path = Jsoo_node.Path.(join [path;"data"]) in
          let loaded = load ~path () in
          assert_ok (empty = loaded)
        );
    ]
end
