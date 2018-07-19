module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {
    context: C.Types.Viewer_context.t;
  }

  let make () = {context = C.Types.Viewer_context.empty}
  let reduce t = function
    | C.Message.Enter_file_tree_context ->
      {context = {C.Types.Viewer_context.in_file_tree = true}}
    | C.Message.Leave_file_tree_context ->
      {context = {C.Types.Viewer_context.in_file_tree = false}}
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
