module T = Sxfiler_domain
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t

  type t =
    { current_mode : C.Types.Mode.t
    ; current_content : C.Types.Mode.content }

  let make () =
    {current_mode = C.Types.Mode.(Content File_tree); current_content = C.Types.Mode.File_tree}

  let reduce t = function
    | C.Message.Change_mode (Complete as mode) -> {t with current_mode = mode}
    | C.Message.Change_mode (Content (_ as content) as mode) ->
      {current_mode = mode; current_content = content}
    | _ -> t

  (** [match_current_mode t ~mode] returns current mode of [t] is same or not [mode] *)
  let match_current_mode t ~mode = t.current_mode = mode

  let is_content_focused {current_mode; _} =
    match current_mode with C.Types.Mode.Complete -> false | C.Types.Mode.Content _ -> true

  let is_complete_focused t = not @@ is_content_focused t
  let current_content {current_content; _} = current_content
  let equal : t -> t -> bool = ( = )
end

module Store = C.Store.Make (State)
