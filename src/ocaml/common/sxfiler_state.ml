module M = Sxfiler_message
module T = Sxfiler_types
module FFI = Sxfiler_ffi
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
