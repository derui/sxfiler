module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make scanner repository with backend. *)
module Make(S:C.Statable.S with type state = D.Condition.t) : D.Condition.Repository = struct
  let resolve () = S.with_lock (fun state -> Lwt.return state)

  let store t = S.with_lock (fun _ -> S.update t)
end
