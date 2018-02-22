module M = Common_message
module T = Common_types
module Config = Common_config
module Thread = Lwt

module Operation = struct
  type t = {
    next: M.Operation.t option;
    confirming: bool;
    executing: bool;
  }

  class type js = object
    method next: M.Operation.t Js.opt Js.readonly_prop
    method confirming: bool Js.t Js.readonly_prop
    method executing: bool Js.t Js.readonly_prop
  end

  let empty = {
    next = None;
    confirming = false;
    executing = false;
  }

  let to_js : t -> js Js.t = fun t -> object%js
    val next = Js.Opt.option t.next
    val confirming = Js.bool t.confirming
    val executing = Js.bool t.executing
  end

  let of_js : js Js.t -> t = fun js -> {
      next = Js.Opt.to_option js##.next;
      confirming = Js.to_bool js##.confirming;
      executing = Js.to_bool js##.executing
    }
end

module Active_pane_pointer = struct
  type t = [`Left | `Right] [@@deriving variants]
  type js = Js.js_string

  let to_js = function
    | `Left as v -> Js.string @@ Variants.to_name v
    | `Right as v -> Js.string @@ Variants.to_name v

  let of_js js =
    match Js.to_string js with
    | v when v = Variants.to_name `Left -> `Left
    | v when v = Variants.to_name `Right -> `Right
    | _ -> failwith "Unknown type"
end

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

(* All state of this application *)
type t = {
  active_pane: Active_pane_pointer.t;
  left_pane: T.Pane.t;
  right_pane: T.Pane.t;
  waiting: bool;
  terminated: bool;
  config: Config.t;
  operation_log: T.Operation_log.t;

  dialog_state: Dialog_state.t;
  operation: Operation.t
}

class type js = object
  method activePane: Active_pane_pointer.js Js.t Js.readonly_prop
  method leftPane: T.Pane.js Js.t Js.readonly_prop
  method rightPane: T.Pane.js Js.t Js.readonly_prop
  method waiting: bool Js.t Js.readonly_prop
  method terminated: bool Js.t Js.readonly_prop
  method config: Config.js Js.t Js.readonly_prop
  method operationLog: T.Operation_log.js Js.t Js.readonly_prop

  method dialogState: Dialog_state.js Js.t Js.readonly_prop
  method operation: Operation.js Js.t Js.readonly_prop
end

let empty =
  let pane () = T.Pane.make ~id:(T.Pane_id.make ()) ~directory:"." () in
  {
    active_pane = `Left;
    left_pane = pane ();
    right_pane = pane ();
    waiting = false;
    terminated = false;
    config = Config.empty;
    operation_log = T.Operation_log.empty;

    dialog_state = Dialog_state.Close;
    operation = Operation.empty;
  }

let to_js : t -> js Js.t = fun t -> object%js
  val activePane = Active_pane_pointer.to_js t.active_pane
  val leftPane = T.Pane.to_js t.left_pane
  val rightPane = T.Pane.to_js t.right_pane
  val waiting = Js.bool t.waiting
  val terminated = Js.bool t.terminated
  val config = Config.to_js t.config
  val operationLog = T.Operation_log.to_js t.operation_log
  val dialogState = Dialog_state.to_js t.dialog_state
  val operation = Operation.to_js t.operation
end

let of_js : js Js.t -> t = fun t ->
  {
    active_pane = Active_pane_pointer.of_js t##.activePane;
    left_pane = T.Pane.of_js t##.leftPane;
    right_pane = T.Pane.of_js t##.rightPane;
    waiting = Js.to_bool t##.waiting;
    terminated = Js.to_bool t##.terminated;
    config = Config.of_js t##.config;
    operation_log = T.Operation_log.of_js t##.operationLog;
    dialog_state = Dialog_state.of_js t##.dialogState;
    operation = Operation.of_js t##.operation
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

let update_pane t pane =
  {t with
   left_pane = if T.Pane.equal pane t.left_pane then pane else t.left_pane;
   right_pane = if T.Pane.equal pane t.right_pane then pane else t.right_pane;
  }
