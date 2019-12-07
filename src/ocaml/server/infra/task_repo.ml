module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make task repository to *)
module Make
    (S : C.Statable.S with type state = C.Root_state.t)
    (Runner : Sxfiler_server_task.Runner.Instance)
    (NS : Notification_service.S) : D.Task.Repository = struct
  let resolve id = S.with_lock (fun state -> C.Root_state.find_task ~id state |> Lwt.return)

  let store task =
    let%lwt _ = S.with_lock (fun state -> C.Root_state.add_task ~task state |> S.update) in
    (* Add task queue *)
    Runner.(Runner.add_task instance ~task)

  let remove t =
    let%lwt _ = S.with_lock (fun state -> C.Root_state.remove_task ~task:t state |> Lwt.return) in
    Runner.(Runner.stop_task instance ~task:t.id);%lwt
    NS.send ~typ:Task_notification.Canceled.typ t.id;%lwt
    Lwt.return_unit
end
