module M = Common_message
module T = Common_types
module Config = Common_config
module Thread = Lwt

module Dialog_state = struct
  type t =
    | Open of T.dialog_type | Close [@@deriving variants]
  class type js = object
    method dialogType: T.dialog_type Js.optdef Js.readonly_prop
  end

  let to_js = function
    | Open typ -> object%js
      val dialogType = Js.Optdef.return typ
    end
    | Close -> object%js
      val dialogType = Js.Optdef.empty
    end

  let of_js : js Js.t -> t = fun js ->
    match Js.Optdef.to_option js##.dialogType with
    | None -> Close
    | Some typ -> Open typ
end

module Interaction_state : sig
  type t

  class type js = object
    method requestedTask: T.Task_request.js Js.opt Js.readonly_prop
    method interacting: bool Js.t Js.readonly_prop
    method executing: bool Js.t Js.readonly_prop
  end

  val empty : t

  val accept_task: t -> T.Task_request.t -> t
  val requested_task: t -> T.Task_request.t option
  val execute: t -> t
  val interacting: t -> bool
  val executing: t -> bool
  val finish: t -> t

  val to_js: t -> js Js.t
  val of_js: js Js.t -> t

end = struct
  type t = {
    requested_task: T.Task_request.t option;
    interacting : bool;
    executing : bool;
  }

  class type js = object
    method requestedTask: T.Task_request.js Js.opt Js.readonly_prop
    method interacting: bool Js.t Js.readonly_prop
    method executing: bool Js.t Js.readonly_prop
  end

  let empty = {requested_task = None; interacting = false; executing = false}

  let accept_task t task = {requested_task = Some task; interacting = true; executing = false}
  let requested_task {requested_task;_} = requested_task
  let interacting {interacting;_} = interacting
  let executing {executing;_} = executing
  let execute t = {t with interacting = false;executing = true}
  let finish t = {requested_task = None;interacting = false;executing = false}

  let to_js t = object%js
    val requestedTask = Js.Opt.option t.requested_task
    val interacting = Js.bool t.interacting
    val executing = Js.bool t.executing
  end

  let of_js (js:js Js.t) = {
    requested_task = Js.Opt.to_option js##.requestedTask;
    interacting = Js.to_bool js##.interacting;
    executing = Js.to_bool js##.executing;
  }
end

(* All state of this application *)
type t = {
  active_pane: T.Pane_location.t;
  left_pane: T.Pane.t;
  right_pane: T.Pane.t;
  waiting: bool;
  terminated: bool;
  config: Config.t;
  operation_log: T.Operation_log.t;
  dialog_state: Dialog_state.t;
  interaction_state: Interaction_state.t;
}

class type js = object
  method activePane: T.Pane_location.js Js.t Js.readonly_prop
  method leftPane: T.Pane.js Js.t Js.readonly_prop
  method rightPane: T.Pane.js Js.t Js.readonly_prop
  method waiting: bool Js.t Js.readonly_prop
  method terminated: bool Js.t Js.readonly_prop
  method config: Config.js Js.t Js.readonly_prop
  method operationLog: T.Operation_log.js Js.t Js.readonly_prop

  method dialogState: Dialog_state.js Js.t Js.readonly_prop
  method interactionState: Interaction_state.js Js.t Js.readonly_prop
end

let empty =
  let pane () = T.Pane.make ~directory:"." () in
  {
    active_pane = `Left;
    left_pane = pane ();
    right_pane = pane ();
    waiting = false;
    terminated = false;
    config = Config.empty;
    operation_log = T.Operation_log.empty;

    dialog_state = Dialog_state.Close;
    interaction_state = Interaction_state.empty;
  }

let to_js : t -> js Js.t = fun t -> object%js
  val activePane = T.Pane_location.to_js t.active_pane
  val leftPane = T.Pane.to_js t.left_pane
  val rightPane = T.Pane.to_js t.right_pane
  val waiting = Js.bool t.waiting
  val terminated = Js.bool t.terminated
  val config = Config.to_js t.config
  val operationLog = T.Operation_log.to_js t.operation_log
  val dialogState = Dialog_state.to_js t.dialog_state
  val interactionState = Interaction_state.to_js t.interaction_state
end

let of_js : js Js.t -> t = fun t ->
  {
    active_pane = T.Pane_location.of_js t##.activePane;
    left_pane = T.Pane.of_js t##.leftPane;
    right_pane = T.Pane.of_js t##.rightPane;
    waiting = Js.to_bool t##.waiting;
    terminated = Js.to_bool t##.terminated;
    config = Config.of_js t##.config;
    operation_log = T.Operation_log.of_js t##.operationLog;
    dialog_state = Dialog_state.of_js t##.dialogState;
    interaction_state = Interaction_state.of_js t##.interactionState;
  }

(* Utility functions *)

module Pane = struct
  (** Get file stats currently selected in [pane] *)
  let pointed_file pane =
    let module P = T.Pane in
    let pos = pane.P.cursor_pos
    and files = Array.of_list pane.P.file_list in
    files.(pos)
end

let is_left_active t = t.active_pane = `Left
let is_right_active t = t.active_pane = `Right
let swap_active_pane t =
  {t with active_pane =
            match t.active_pane with
            | `Left -> `Right
            | `Right -> `Left
  }

let active_pane t =
  if is_left_active t then t.left_pane else t.right_pane
let inactive_pane t =
  if is_left_active t then t.right_pane else t.left_pane

let update_pane t ~loc ~pane =
  {t with
   left_pane = if loc = T.Pane_location.left then pane else t.left_pane;
   right_pane = if loc = T.Pane_location.right then pane else t.right_pane;
  }
