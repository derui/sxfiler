(** State definitions for application. *)
module T = Sxfiler_types
module C = Sxfiler_renderer_core

module Viewer_stacks = struct
  type message = C.Message.t
  type t = {
    file_tree: C.Types.File_tree.t;
  }

  let empty () = {file_tree = C.Types.File_tree.make Const.(scanner_1, scanner_2);}
  let reduce t = function
    | C.Message.Update_scanner scanner ->
      let file_tree = C.Types.File_tree.update t.file_tree ~scanner in
      {file_tree}
end

module Config = struct
  type message = C.Message.t
  type t = T.Configuration.t

  let empty () = T.Configuration.default
  let reduce t _ = t
end

module Layout = struct
  type message = C.Message.t
  type t = string list

  let empty () = [Const.workspace_1;Const.workspace_2]
  let reduce t _ = t
end
