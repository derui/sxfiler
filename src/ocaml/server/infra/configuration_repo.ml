module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make configuration repository with backend. *)
module Make (S : C.Statable.S with type state = C.Root_state.t) : D.Configuration.Repository =
struct
  let resolve () = S.with_lock (fun state -> Lwt.return @@ state.C.Root_state.configuration)

  let store configuration =
    S.with_lock (fun state -> S.update {state with C.Root_state.configuration})
end
