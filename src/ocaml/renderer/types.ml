module T = Sxfiler_types

module Viewer = struct
  module File_tree = struct
    type t = {
      snapshot: T.Tree_snapshot.t;
      selected_item_index: int;
    }
  end

  type t =
    | File_tree of File_tree.t
end

(** Viewer_state has some common state for Viewer. Each viewers are related this. *)
module Viewer_state = struct
  type t = {
    viewer: Viewer.t;
    closing: bool;              (* viewer is closing now. *)
    closed: bool;               (* viewer is closed. *)
  }
end

module Viewer_stack = struct
  type t = Viewer_state.t list
end
