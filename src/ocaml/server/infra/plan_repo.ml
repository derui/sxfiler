module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make plan repository with backend. *)
module Make (S : C.Statable.S with type state = C.Root_state.t) : D.Plan.Repository = struct
  let resolve id = S.with_lock (fun state -> Lwt.return @@ C.Root_state.find_plan ~id state)

  let store plan =
    S.with_lock (fun state ->
        let state = C.Root_state.add_plan ~plan state in
        S.update state )

  let remove plan =
    S.with_lock (fun state ->
        let state = C.Root_state.remove_plan ~plan state in
        S.update state )
end
