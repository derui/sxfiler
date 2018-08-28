module T = Sxfiler_domain
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {current_mode : C.Types.Mode.t}

  let make () = {current_mode = C.Types.Mode.File_tree}
  let reduce t = function C.Message.Change_mode mode -> {current_mode = mode} | _ -> t

  (** [match_current_mode t ~mode] returns current mode of [t] is same or not [mode] *)
  let match_current_mode t ~mode = t.current_mode = mode

  let equal : t -> t -> bool = ( = )
end

module Store = C.Store.Make (State)
