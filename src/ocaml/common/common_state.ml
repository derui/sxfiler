module M = Common_message
module T = Common_types
module Config = Common_config
module Thread = Lwt

module Dialog = struct
  type t = {
    typ: T.dialog_type option;
    opening: bool;
  }

  class type js = object
    method typ: T.dialog_type Js.opt Js.readonly_prop
    method opening: bool Js.t Js.readonly_prop
  end

  let empty = {
    typ = None;
    opening = false;
  }

  let to_js : t -> js Js.t = fun t -> object%js
    val typ = Js.Opt.option t.typ
    val opening = Js.bool t.opening
  end

  let of_js : js Js.t -> t = fun js -> {
      typ = Js.Opt.to_option js##.typ;
      opening = Js.to_bool js##.opening;
    }
end

(* All state of this application *)
type t = {
  panes: T.Pane.t array;
  current_pane: T.Pane_id.t;
  waiting: bool;
  terminated: bool;
  config: Config.t;
  operation_log: T.Operation_log.t;

  dialog_state: Dialog.t;
}

class type js = object
  method panes: T.Pane.js Js.t Js.js_array Js.t Js.readonly_prop
  method currentPane: T.Pane_id.js Js.t Js.readonly_prop
  method waiting: bool Js.t Js.readonly_prop
  method terminated: bool Js.t Js.readonly_prop
  method config: Config.js Js.t Js.readonly_prop
  method operationLog: T.Operation_log.js Js.t Js.readonly_prop

  method dialogState: Dialog.js Js.t Js.readonly_prop
end

let empty =
  let pane_size = 2 in
  let pane () = T.Pane.make ~id:(T.Pane_id.make ()) ~directory:"." () in
  let panes = Array.init pane_size (fun _ -> pane ()) in
  {
    panes;
    current_pane = panes.(0).id;
    waiting = false;
    terminated = false;
    config = Config.empty;
    operation_log = T.Operation_log.empty;
    dialog_state = Dialog.empty;
  }

let to_js : t -> js Js.t = fun t -> object%js
  val panes = Array.map T.Pane.to_js t.panes |> Js.array
  val currentPane = T.Pane_id.to_js t.current_pane
  val waiting = Js.bool t.waiting
  val terminated = Js.bool t.terminated
  val config = Config.to_js t.config
  val operationLog = T.Operation_log.to_js t.operation_log
  val dialogState = Dialog.to_js t.dialog_state
end

let of_js : js Js.t -> t = fun t ->
  let panes = Array.map T.Pane.of_js @@ Js.to_array t##.panes in
  {
    panes;
    current_pane = T.Pane_id.of_js t##.currentPane;
    waiting = Js.to_bool t##.waiting;
    terminated = Js.to_bool t##.terminated;
    config = Config.of_js t##.config;
    operation_log = T.Operation_log.of_js t##.operationLog;
    dialog_state = Dialog.of_js t##.dialogState
  }


(* Utility functions *)

module Panes = struct
  (**  *)
  let select_pane panes id =
    let module P = T.Pane in
    Array.to_list panes |> List.find_opt (fun v -> T.Pane_id.equal v.P.id id)

  let select_other_pane panes id =
    let module P = T.Pane in
    Array.to_list panes |> List.find_opt (fun v -> not @@ T.Pane_id.equal v.P.id id)

  (** Get file stats currently selected in [pane] *)
  let pointed_file pane =
    let module P = T.Pane in
    let pos = pane.P.cursor_pos
    and files = Array.of_list pane.P.file_list in
    files.(pos)

  (** Replace the [pane] having same id in [panes] *)
  let replace_pane panes pane =
    let module P = T.Pane in
    match select_pane panes pane.P.id with
    | Some pane ->
      Array.map (fun v -> if T.Pane_id.equal v.P.id pane.P.id then pane else v) panes
    | None -> Array.concat [panes;[|pane|]]

end

(** Get current pane from state *)
let current_pane state =
  let current_id = state.current_pane in
  match Panes.select_pane state.panes current_id with
  | None -> assert false
  | Some pane -> pane

(** Get directory of current pane of [state]. *)
let current_pane_directory state =
  let current_id = state.current_pane in
  let module P = T.Pane in
  match Panes.select_pane state.panes current_id with
  | None -> Js.string ""
  | Some pane -> Js.string pane.P.directory
