module C = Sxfiler_server_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator

let filer_updated_api =
  {
    Notification_service.to_method = (fun _ -> "notification/filerUpdated");
    to_json = (fun v -> Tr.Filer.(of_domain v |> to_json));
  }

(** Make filer repository with backend. *)
module Make (S : C.Statable.S with type state = C.Root_state.t) (NS : Notification_service.S) :
  D.Filer.Repository = struct
  let resolve id = S.with_lock (fun state -> Lwt.return @@ C.Root_state.find_filer ~id state)

  let resolve_by_name name =
    S.with_lock (fun state -> Lwt.return @@ C.Root_state.find_filer_by_name ~name state)

  let store filer =
    S.with_lock (fun state ->
        let state = C.Root_state.add_filer ~filer state in
        let%lwt () = S.update state in
        NS.send ~typ:filer_updated_api filer)
end
