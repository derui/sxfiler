module Ty = Sxfiler_types

module Viewer = struct
  module File_tree = struct
    type t = {
      scanner: Ty.Scanner.t;
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

  let make viewer = {
    viewer;
    closing = false;
    closed = false;
  }
end

(** {!Viewer_stack} allows to stack modules that are created from {!Viewer_state.S}.  *)
module Viewer_stack = struct
  type t = Viewer_state.t list

  let empty () = []
  let push t ~v = v :: t
  let pop = function
    | [] -> None
    | _ :: rest -> Some rest
end
