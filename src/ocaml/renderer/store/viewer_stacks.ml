module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {
    file_tree: C.Types.File_tree.t;
  }

  let make order = {file_tree = C.Types.File_tree.make order;}
  let reduce t = function
    | C.Message.Update_scanner scanner ->
      let file_tree = C.Types.File_tree.update t.file_tree ~scanner in
      {file_tree}

  let equal _ _ = false
end

module Store = C.Store.Make(State)
