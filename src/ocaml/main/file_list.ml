module T = Sxfiler_common.Types
module Path = Jsoo_node.Path
module Fs = Jsoo_node.Fs

let assert_directory path =
  let open Minimal_monadic_caml.Result.Infix in
  match (Fs.statSync path >>= fun stat -> Ok stat##isDirectory) with
  | Ok v -> ()
  | Error `JsooSystemError e -> begin
      let module E = Jsoo_node.Errors.System_error in
      match E.to_code e with
      | E.ENOENT | E.ENOTDIR -> ()
      | _ -> raise (Errors.to_error @@ `Sxfiler_not_directory path)
    end

(**
 * Get files in the directory.
 *
 * Caution, this method needs the fs module of execution environment, such as fs on node or
 * original-fs on electron.
*)
let get_file_stats ~fs path =
  assert_directory path;

  let open Lwt.Infix in
  let filename_to_stats names =
    let names = Array.map (fun v -> Path.join [path; v]) names |> Array.to_list in
    let linked_to filename stat =
      if Js.to_bool stat##.isSymbolicLink then begin
        match Fs.readlinkSync filename with
        | Ok link_path -> Some link_path
        | _ -> None
      end
      else None
    in
    let get_file_stat filename =
      let open Minimal_monadic_caml.Result.Infix in
      Fs.lstatSync filename
      >>= fun stat ->
          let stat = Fs.stat_to_obj stat in
          let link_path = linked_to filename stat in
          let directory = Path.dirname filename in
          let filename = Path.basename filename in
          Ok (T.File_stat.make ~filename ~stat ~link_path ~directory)
    in
    List.map get_file_stat names
  in

  Lwt_js.yield ()
  >>= Lwt.wrap1 (fun () ->
      let open Minimal_monadic_caml.Result.Infix in
      Fs.readdirSync path
      >>= fun names -> let stats = filename_to_stats names in
      let rec list_to_result ret list =
        match list with
        | [] -> Ok (List.rev ret)
        | stat :: rest -> begin
            match stat with
            | Ok stat -> list_to_result (stat :: ret) rest
            | Error _ as e -> e
          end
      in
      list_to_result [] stats
    )
