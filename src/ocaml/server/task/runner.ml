(** This module will run tasks with task queue. *)

module D = Sxfiler_domain
module Log = (val Sxfiler_server_core.Logger.make [ "task" ])
include Runner_intf

module Task_state = Set.Make (struct
  type t = Uuidm.t

  let compare = Uuidm.compare
end)

module Subscriber = struct
  type t = {
    id : Uuidm.t;
    f : subscriber;
  }

  let compare v1 v2 = Uuidm.compare v1.id v2.id
end

module Subscriber_set = Set.Make (Subscriber)

module Impl (R : D.Id_generator_intf.Gen_random with type id = Uuidm.t) = struct
  type thread_state =
    [ `Accepted of D.Task.t
    | `Rejected
    ]
  (** only entry point to add task to task queue in this module. *)

  type t = {
    task_queue : D.Task.t Lwt_stream.t;
    task_queue_writer : D.Task.t option -> unit;
    mutable task_state : Task_state.t;
    task_state_lock : Lwt_mutex.t;
    task_mailbox : thread_state Lwt_mvar.t;
    wakener : unit Lwt.u;
    waiter : unit Lwt.t;
    handler_lock : Lwt_mutex.t;
    mutable subscribers : Subscriber_set.t;
    subscribers_lock : Lwt_mutex.t;
  }

  (** Forever loop to accept task. Running this function must be in other thread of worker thread. *)
  let accept_task_loop t =
    let rec loop () =
      let open Lwt in
      Log.info (fun m -> m "Waiting task entry...");%lwt
      let%lwt task = Lwt_mvar.take t.task_mailbox in
      match task with
      | `Accepted task ->
          Log.info (fun m -> m "Task accepted");%lwt
          Lwt.return @@ t.task_queue_writer (Some task) >>= loop
      | `Rejected ->
          Log.info (fun m -> m "Rejected");%lwt
          Lwt.return_unit
    in
    loop ()

  let add_state t id =
    Lwt_mutex.with_lock t.task_state_lock (fun () ->
        t.task_state <- Task_state.add id t.task_state;
        Lwt.return_unit)

  let remove_state t id =
    Lwt_mutex.with_lock t.task_state_lock (fun () ->
        t.task_state <- Task_state.remove id t.task_state;
        Lwt.return_unit)

  (** Forever loop to run task. *)
  let run_task_loop t () =
    let module C = Sxfiler_server_core in
    t.task_queue
    |> Lwt_stream.iter_p (fun task ->
           let%lwt () = add_state t task.D.Task.id in
           Lwt.finalize
             (fun () ->
               Log.info (fun m -> m "Start executing task [%s]..." Uuidm.(to_string task.id));%lwt
               let%lwt () = D.Task.(execute task) in
               Log.info (fun m -> m "Finish executing task [%s]" Uuidm.(to_string task.id)))
             (fun () ->
               let%lwt () =
                 Lwt_mutex.with_lock t.task_state_lock (fun () ->
                     Subscriber_set.to_seq t.subscribers
                     |> Seq.map (fun v -> v.Subscriber.f task)
                     |> List.of_seq |> Lwt.join)
               in
               remove_state t task.D.Task.id))

  (** [add_task task] add [task] to mailbox of task accepter. *)
  let add_task t ~task = Lwt_mvar.put t.task_mailbox (`Accepted task)

  let subscribe t ~f =
    let id = R.generate () in
    let f' = { Subscriber.id; f } in
    let unsubscribe () =
      Lwt_mutex.with_lock t.subscribers_lock (fun () ->
          t.subscribers <- Subscriber_set.remove f' t.subscribers;
          Lwt.return_unit)
    in
    Lwt_mutex.with_lock t.task_state_lock (fun () ->
        t.subscribers <- Subscriber_set.add f' t.subscribers;
        Lwt.return_unit);%lwt
    Lwt.return unsubscribe

  let start t =
    let module C = Sxfiler_server_core in
    let accepter = accept_task_loop t in
    let worker = run_task_loop t () in
    let stopper =
      let%lwt () = t.waiter in
      (* Cancel all async threads. *)
      Lwt_mvar.put t.task_mailbox `Rejected
    in
    Log.info (fun m -> m "Task runner started");%lwt
    let open Lwt in
    accepter <?> worker <?> stopper >>= fun () -> Log.info (fun m -> m "Task runner finished")

  let stop t = Lwt.wakeup t.wakener ()
end

let make (module R : D.Id_generator_intf.Gen_random with type id = Uuidm.t) =
  let module Runner = Impl (R) in
  let task_queue, task_queue_writer = Lwt_stream.create () in
  let task_state_lock = Lwt_mutex.create () in
  let subscribers_lock = Lwt_mutex.create () in
  let subscribers = Subscriber_set.empty in
  let task_state = Task_state.empty in
  let task_mailbox = Lwt_mvar.create_empty () in
  let waiter, wakener = Lwt.task () in
  let handler_lock = Lwt_mutex.create () in
  ( module struct
    module Runner = Runner

    let instance =
      {
        Runner.task_queue;
        task_queue_writer;
        task_state;
        task_state_lock;
        task_mailbox;
        waiter;
        wakener;
        handler_lock;
        subscribers;
        subscribers_lock;
      }
  end : Instance )
