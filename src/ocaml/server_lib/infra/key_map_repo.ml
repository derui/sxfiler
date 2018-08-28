module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make scanner repository with backend. *)
module Make (S : C.Statable.S with type state = string D.Key_map.t) :
  D.Key_map_repository.S with type value = string = struct
  type value = string

  let resolve () = S.with_lock (fun state -> Lwt.return state)
  let store keymap = S.with_lock (fun _ -> S.update keymap)
end
