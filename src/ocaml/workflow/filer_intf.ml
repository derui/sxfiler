open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_dependency

type side =
  | Left
  | Right
[@@deriving eq, show]

type direction =
  | Left_to_right
  | Right_to_left
[@@deriving eq, show]

type event =
  | Initialized      of D.Filer.t
  | Location_changed of (side * D.File_window.free D.File_window.t)
  | Updated          of (side * D.File_window.free D.File_window.t)
[@@deriving eq, show]

type transfer_target = D.File_item.Id.t [@@deriving eq, show]

type transfer_status =
  | Success
  | Failed
  | Canceled
[@@deriving eq, show]

type transfer_result = {
  source : Path.t;
  dest : Path.t;
  status : transfer_status;
  timestamp : Time.t;
}
[@@deriving eq, show]

type delete_result = {
  item : D.File_item.t;
  timestamp : Time.t;
}
[@@deriving eq, show]

(** move location of file list placed on left side *)
module Move_location = struct
  type error = Not_initialized

  type input = {
    location : Path.t;
    side : side;
  }

  type 'a work_flow = input -> ((event list, error) result, 'a) S.t
end

(** the workflow to initialize filer from locations *)
module Initialize = struct
  type input = {
    left_location : Path.t;
    right_location : Path.t;
    left_history : D.Location_history.t option;
    right_history : D.Location_history.t option;
    left_sort_order : D.Types.Sort_type.t;
    right_sort_order : D.Types.Sort_type.t;
  }

  type 'a work_flow = input -> (event list, 'a) S.t
end

(** the workflow to reload all file list in the filer *)
module Reload_all = struct
  type error = Not_initialized

  type input = unit

  type 'a work_flow = input -> ((event list, error) result, 'a) S.t
end

(** the workflow to copy item from one side to another side *)
module Copy = struct
  type error = Not_initialized

  type input = {
    direction : direction;
    target : transfer_target;
  }

  type output = {
    events : event list;
    result : transfer_result;
  }

  type 'a work_flow = input -> ((output, error) result, 'a) S.t
end

module Move = struct
  type error = Not_initialized

  type input = {
    direction : direction;
    target : transfer_target;
  }
  [@@deriving eq, show]

  type output = {
    events : event list;
    result : transfer_result;
  }
  [@@deriving eq, show]

  type 'a work_flow = input -> ((output, error) result, 'a) S.t
end

module Delete = struct
  type error = Not_initialized

  type input = {
    side : side;
    target : transfer_target;
  }

  type output = {
    result : delete_result option;
    events : event list;
  }

  type 'a work_flow = input -> ((output, error) result, 'a) S.t
end

module Open_node = struct
  type error =
    | Not_initialized
    | Item_not_found      of D.File_item.Id.t
    | Location_not_exists of Path.t

  type output =
    | Not_implemented
    | Open_directory  of event list

  type input = {
    side : side;
    item_id : D.File_item.Id.t;
  }

  type 'a work_flow = input -> ((output, error) result, 'a) S.t
end

(** up directory of specified side *)
module Up_directory = struct
  type error = Not_initialized

  type input = { side : side }

  type 'a work_flow = input -> ((event list, error) result, 'a) S.t
end

(** toggle mark of the item *)
module Toggle_mark = struct
  type error =
    | Item_not_found
    | Not_initialized

  type input = {
    side : side;
    item_id : D.File_item.Id.t;
  }

  type 'a work_flow = input -> ((event list, error) result, 'a) S.t
end

type commands =
  | Initialize    of Initialize.input
  | Move_location of Move_location.input
  | Reload_all    of Reload_all.input
  | Copy          of Copy.input
  | Move          of Move.input
  | Delete        of Delete.input
  | Open_node     of Open_node.input
  | Up_directory  of Up_directory.input
  | Toggle_mark   of Toggle_mark.input
