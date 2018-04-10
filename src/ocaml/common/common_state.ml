module M = Common_message
module T = Common_types
module PH = Common_pane_history
module Config = Common_config
module Thread = Lwt

open Common_intf

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

module Bookmarks = struct
  type t = string list

  class type js = object
    method data: Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  end

  let empty = []
  let toggle state file =
    if List.exists ((=) file) state
    then state
    else file :: state

  let to_js t = object%js
    val data = List.map Js.string t |> Array.of_list |> Js.array
  end

  let of_js js =
    Js.to_array js##.data |> Array.to_list |> List.map Js.to_string

end

module Task_state : sig
  type t

  class type js = object
    method requestedTask: T.Task_request.js Js.t Js.opt Js.readonly_prop
    method executing: bool Js.t Js.readonly_prop
  end

  val empty : t

  val accept_task: t -> T.Task_request.t -> t
  val requested_task: t -> T.Task_request.t option
  val executing: t -> bool
  val finish: t -> t

  val to_js: t -> js Js.t
  val of_js: js Js.t -> t

end = struct
  type t = {
    requested_task: T.Task_request.t option;
    executing : bool;
  }

  class type js = object
    method requestedTask: T.Task_request.js Js.t Js.opt Js.readonly_prop
    method executing: bool Js.t Js.readonly_prop
  end

  let empty = {requested_task = None; executing = false}

  let accept_task t task = {requested_task = Some task; executing = true}
  let requested_task {requested_task;_} = requested_task
  let executing {executing;_} = executing
  let finish t = {requested_task = None;executing = false}

  let to_js t = object%js
    val requestedTask = Js.Opt.map (Js.Opt.option t.requested_task) T.Task_request.to_js
    val executing = Js.bool t.executing
  end

  let of_js (js:js Js.t) = {
    requested_task = Js.Opt.to_option @@ Js.Opt.map js##.requestedTask T.Task_request.of_js;
    executing = Js.to_bool js##.executing;
  }
end

module Make_comp_state(I:Completion_item):
  Completion_state with type item := I.t
                    and type item_js := I.js = struct
  type t = {
    cursor_pos: T.cursor_pos;
    items: I.t array;
    candidates: I.t array;
    completing: bool;
    prev_input: string;
  }

  class type js = object
    method cursorPos: int Js.readonly_prop
    method items: I.js Js.t Js.js_array Js.t Js.readonly_prop
    method candidates: I.js Js.t Js.js_array Js.t Js.readonly_prop
    method completing: bool Js.t Js.readonly_prop
    method prevInput: Js.js_string Js.t Js.readonly_prop
  end

  let empty = {
    cursor_pos = 0;
    items = [||];
    candidates = [||];
    completing = false;
    prev_input = "";
  }

  let select_next t = {t with cursor_pos = min (Array.length t.items - 1) (succ t.cursor_pos)}
  let select_prev t = {t with cursor_pos = max 0 (pred t.cursor_pos)}

  let refresh t ~candidates = {t with candidates;items = candidates;cursor_pos = 0}
  let complete t ~input ~items = {t with items; prev_input = input}
  let start_completion t = {t with completing = true}
  let finish_completion t = {t with completing = false}

  let selected t =
    let cursor_pos = min ((Array.length t.items) - 1) @@ max 0 t.cursor_pos in
    Array.get t.items cursor_pos

  let to_js : t -> js Js.t = fun t -> object%js
    val cursorPos = t.cursor_pos
    val items = Array.map I.to_js t.items |> Js.array
    val candidates = Array.map I.to_js t.candidates |> Js.array
    val completing = Js.bool t.completing
    val prevInput = Js.string t.prev_input
  end

  let of_js : js Js.t -> t = fun js -> {
      cursor_pos = js##.cursorPos;
      items = Js.to_array js##.items |> Array.map I.of_js;
      candidates = Js.to_array js##.candidates |> Array.map I.of_js;
      completing = Js.to_bool js##.completing;
      prev_input = Js.to_string js##.prevInput;
    }
end

module File_completion_state = Make_comp_state(T.File_stat)
module History_completion_state = Make_comp_state(Common_pane_history.History)

(* All state of this application *)
type t = {
  active_pane: T.Pane_location.t;
  left_pane: T.Pane.t;
  right_pane: T.Pane.t;
  left_pane_history: PH.t;
  right_pane_history: PH.t;
  waiting: bool;
  terminated: bool;
  config: Config.t;
  operation_log: T.Operation_log.t;
  dialog_state: Dialog_state.t;
  task_state: Task_state.t;
  bookmarks: Bookmarks.t;

  completing: T.completion_type option;

  file_completion_state: File_completion_state.t;
  history_completion_state: History_completion_state.t;
}

class type js = object
  method activePane: T.Pane_location.js Js.t Js.readonly_prop
  method leftPane: T.Pane.js Js.t Js.readonly_prop
  method rightPane: T.Pane.js Js.t Js.readonly_prop
  method leftPaneHistory: PH.js Js.t Js.readonly_prop
  method rightPaneHistory: PH.js Js.t Js.readonly_prop
  method waiting: bool Js.t Js.readonly_prop
  method terminated: bool Js.t Js.readonly_prop
  method config: Config.js Js.t Js.readonly_prop
  method operationLog: T.Operation_log.js Js.t Js.readonly_prop
  method dialogState: Dialog_state.js Js.t Js.readonly_prop
  method taskState: Task_state.js Js.t Js.readonly_prop
  method completing: T.completion_type Js.opt Js.readonly_prop
  method bookmarks: Bookmarks.js Js.t Js.readonly_prop
  method fileCompletionState: File_completion_state.js Js.t Js.readonly_prop
  method historyCompletionState: History_completion_state.js Js.t Js.readonly_prop
end

let empty =
  let pane () = T.Pane.make ~directory:"." () in
  {
    active_pane = `Left;
    left_pane = pane ();
    right_pane = pane ();
    left_pane_history = PH.make ();
    right_pane_history = PH.make ();
    waiting = false;
    terminated = false;
    config = Config.empty;
    operation_log = T.Operation_log.empty;

    dialog_state = Dialog_state.Close;
    task_state = Task_state.empty;
    bookmarks = Bookmarks.empty;
    completing = None;
    file_completion_state = File_completion_state.empty;
    history_completion_state = History_completion_state.empty;
  }

let to_js : t -> js Js.t = fun t -> object%js
  val activePane = T.Pane_location.to_js t.active_pane
  val leftPane = T.Pane.to_js t.left_pane
  val rightPane = T.Pane.to_js t.right_pane
  val leftPaneHistory = PH.to_js t.left_pane_history
  val rightPaneHistory = PH.to_js t.right_pane_history
  val waiting = Js.bool t.waiting
  val terminated = Js.bool t.terminated
  val config = Config.to_js t.config
  val operationLog = T.Operation_log.to_js t.operation_log
  val dialogState = Dialog_state.to_js t.dialog_state
  val taskState = Task_state.to_js t.task_state
  val completing = Js.Opt.option t.completing
  val bookmarks = Bookmarks.to_js t.bookmarks
  val fileCompletionState = File_completion_state.to_js t.file_completion_state
  val historyCompletionState = History_completion_state.to_js t.history_completion_state
end

let of_js : js Js.t -> t = fun t ->
  {
    active_pane = T.Pane_location.of_js t##.activePane;
    left_pane = T.Pane.of_js t##.leftPane;
    right_pane = T.Pane.of_js t##.rightPane;
    left_pane_history = PH.of_js t##.leftPaneHistory;
    right_pane_history = PH.of_js t##.rightPaneHistory;
    waiting = Js.to_bool t##.waiting;
    terminated = Js.to_bool t##.terminated;
    config = Config.of_js t##.config;
    operation_log = T.Operation_log.of_js t##.operationLog;
    dialog_state = Dialog_state.of_js t##.dialogState;
    task_state = Task_state.of_js t##.taskState;
    completing = Js.Opt.to_option t##.completing;
    bookmarks = Bookmarks.of_js t##.bookmarks;
    file_completion_state = File_completion_state.of_js t##.fileCompletionState;
    history_completion_state = History_completion_state.of_js t##.historyCompletionState;
  }

(* Utility functions *)

module Pane = struct
  (** Get file stats currently selected in [pane].
      This function return list that contains focused item if no marked item,
      or else marked items
  *)
  let selected_files pane =
    let module P = T.Pane in
    let open Minimal_monadic_caml.Option in
    let open Infix in
    let items = match pane.P.marked_items with
      | [] -> P.find_item ~id:pane.P.focused_item pane
        >>= lift @@ fun v -> [v]
      | _ as items -> Some (List.map snd items)
    in
    Common_util.Option.get ~default:[] items
end

let is_left_active t = t.active_pane = `Left
let is_right_active t = t.active_pane = `Right
let swap_active_pane t =
  {t with active_pane = match t.active_pane with
       | `Left -> `Right
       | `Right -> `Left
  }

let active_pane_history t =
  if is_left_active t then t.left_pane_history else t.right_pane_history

let pane_history t ~loc =
  match loc with
  | `Left -> t.left_pane_history
  | `Right -> t.right_pane_history

let update_pane_history t ~loc ~history =
  {t with
   left_pane_history = if loc = T.Pane_location.left then history else t.left_pane_history;
   right_pane_history = if loc = T.Pane_location.right then history else t.right_pane_history;
  }

let active_pane t =
  if is_left_active t then t.left_pane else t.right_pane
let inactive_pane t =
  if is_left_active t then t.right_pane else t.left_pane

let update_pane ?loc ~pane t =
  let loc' = match loc with
    | None -> t.active_pane
    | Some loc -> loc
  in
  {t with
   left_pane = if loc' = T.Pane_location.left then pane else t.left_pane;
   right_pane = if loc' = T.Pane_location.right then pane else t.right_pane;
  }

let is_completing {completing;_} = match completing with
  | None -> false
  | Some _ -> true
