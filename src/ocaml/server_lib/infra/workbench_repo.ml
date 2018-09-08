module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make filer repository with backend. *)
module Make (S : C.Statable.S with type state = C.Workbench_state.t) : D.Workbench.Repository =
struct
  let resolve id = S.with_lock (fun state -> Lwt.return @@ C.Workbench_state.find ~id state)

  let store wb =
    S.with_lock (fun state ->
        let state = C.Workbench_state.add ~value:wb state in
        S.update state )

  let remove wb =
    S.with_lock (fun state ->
        let state = C.Workbench_state.remove ~id:wb.D.Workbench.id state in
        S.update state )
end
