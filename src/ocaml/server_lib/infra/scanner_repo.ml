module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make scanner repository with backend. *)
module Make(S:C.Statable.S with type state = C.Root_state.t) : D.Scanner.Repository = struct
  let resolve id =
    S.with_lock (fun state ->
        Lwt.return @@ C.Root_state.find_scanner ~id state
      )

  let store scanner =
    S.with_lock (fun state ->
        let state = C.Root_state.add_scanner ~scanner state in
        S.update state)
end
