module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make filer repository with backend. *)
module Make (S : C.Statable.S with type state = C.Root_state.t) : D.Filer.Repository = struct
  let resolve id = S.with_lock (fun state -> Lwt.return @@ C.Root_state.find_filer ~id state)

  let resolve_by_name name =
    S.with_lock (fun state -> Lwt.return @@ C.Root_state.find_filer_by_name ~name state)

  let store filer =
    S.with_lock (fun state ->
        let state = C.Root_state.add_filer ~filer state in
        S.update state )
end
