module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make configuration repository with backend. *)
module Make (S : C.Statable.S with type state = D.Configuration.t) : D.Configuration.Repository =
struct
  let resolve () = S.with_lock (fun state -> Lwt.return state)
  let store configuration = S.with_lock (fun _ -> S.update configuration)
end
