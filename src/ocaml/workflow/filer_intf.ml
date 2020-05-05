open Sxfiler_core
module D = Sxfiler_domain

type side =
  | Left
  | Right
[@@deriving eq, show]

type direction =
  | Left_to_right
  | Right_to_left
[@@deriving eq, show]

type event =
  | Updated             of D.Filer.t
  | Updated_file_window of (side * D.File_window.free D.File_window.t)
[@@deriving eq, show]

type transfer_target =
  | Marked
  | One    of D.File_item.Id.t
[@@deriving eq, show]

(** move location of file list placed on left side *)
module Move_location = struct
  type error = Not_initialized

  type input = {
    location : Path.t;
    side : side;
    filer : D.Filer.t option;
  }

  type work_flow = input -> (event list, error) result Lwt.t
end

(** the workflow to initialize filer from locations *)
module Initialize = struct
  type input = {
    left_location : Path.t;
    right_location : Path.t;
    left_history : D.Location_history.t option;
    right_history : D.Location_history.t option;
  }

  type work_flow = input -> event list Lwt.t
end

(** the workflow to reload all file list in the filer *)
module Reload_all = struct
  type error = Not_initialized

  type input = D.Filer.t option

  type work_flow = input -> (event list, error) result Lwt.t
end

(** the workflow to copy item from one side to another side *)
module Copy = struct
  type input = {
    direction : direction;
    filer : D.Filer.t;
    target : transfer_target;
  }

  type work_flow = input -> event list Lwt.t
end

module Move = struct
  type input = {
    direction : direction;
    filer : D.Filer.t;
    target : transfer_target;
  }
  [@@deriving eq, show]

  type work_flow = input -> event list Lwt.t
end

module Delete = struct
  type input = {
    side : side;
    filer : D.Filer.t;
    target : transfer_target;
  }

  type work_flow = input -> event list Lwt.t
end

module Open_node = struct
  type error =
    | Item_not_found      of D.File_item.Id.t
    | Location_not_exists of Path.t

  type output =
    | Not_implemented
    | Open_directory  of event list

  type input = {
    side : side;
    filer : D.Filer.t;
    item_id : D.File_item.Id.t;
  }

  type work_flow = input -> (output, error) result Lwt.t
end

(** up directory of specified side *)
module Up_directory = struct
  type error = Not_initialized

  type input = {
    side : side;
    filer : D.Filer.t option;
  }

  type work_flow = input -> (event list, error) result Lwt.t
end

(** toggle mark of the item *)
module Toggle_mark = struct
  type error =
    | Item_not_found
    | Not_initialized

  type input = {
    side : side;
    item_id : D.File_item.Id.t;
    filer : D.Filer.t option;
  }

  type work_flow = input -> (event list, error) result Lwt.t
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
