(** This module will run tasks with task queue.  *)

include Runner_intf

module Impl = struct
  (** only entry point to add task to task queue in this module. *)
  type thread_state = [`Accepted of (module Task.Instance) | `Rejected]

  module Task_state = Set.Make (struct
      type t = Uuidm.t

      let compare = Uuidm.compare
    end)

  type t =
    { task_queue : (module Task.Instance) Lwt_stream.t
    ; task_queue_writer : (module Task.Instance) option -> unit
    ; mutable task_state : Task_state.t
    ; task_state_lock : Lwt_mutex.t
    ; task_mailbox : thread_state Lwt_mvar.t
    ; wakener : unit Lwt.u
    ; waiter : unit Lwt.t
    ; handler_lock : Lwt_mutex.t }

  (** Forever loop to accept task. Running this function must be in other thread of worker thread. *)
  let rec accept_task_loop t () =
    let%lwt task = Lwt_mvar.take t.task_mailbox in
    match%lwt Lwt.return task with
    | `Accepted task ->
      t.task_queue_writer (Some task) ; accept_task_loop t ()
    | `Rejected ->
      Lwt.return_unit


  let remove_state t id =
    Lwt_mutex.with_lock t.task_state_lock (fun () ->
        t.task_state <- Task_state.remove id t.task_state ;
        Lwt.return_unit )


  (** Forever loop to run task. *)
  let run_task_loop t () =
    let module C = Sxfiler_server_core in
    Lwt_stream.iter_p
      (fun task ->
         let id = Uuidm.v4_gen (Random.get_state ()) () in
         let%lwt _ =
           Lwt_mutex.with_lock t.task_state_lock (fun () ->
               t.task_state <- Task_state.add id t.task_state ;
               Lwt.return_unit )
         in
         let module Current_task = (val task : Task.Instance) in
         Lwt.return
         @@ Lwt.async (fun () ->
             try%lwt
               let%lwt () = Current_task.(Task.run this) in
               remove_state t id
             with _ -> remove_state t id ) )
      t.task_queue


  (** [add_task task] add [task] to mailbox of task accepter. *)
  let add_task t task = Lwt_mvar.put t.task_mailbox (`Accepted task)

  let start t =
    let module C = Sxfiler_server_core in
    let accepter = accept_task_loop t () in
    let worker = run_task_loop t () in
    let stopper =
      let%lwt () = t.waiter in
      (* Cancel all async threads. *)
      let%lwt () = Lwt_mvar.put t.task_mailbox `Rejected in
      t.task_queue_writer None ;
      Lwt.join [accepter; worker]
    in
    Lwt.async (fun () -> accepter) ;
    Lwt.async (fun () -> worker) ;
    stopper


  let stop t = Lwt.wakeup t.wakener ()
end

let make () =
  let task_queue, task_queue_writer = Lwt_stream.create () in
  let task_state_lock = Lwt_mutex.create () in
  let task_state = Impl.Task_state.empty in
  let task_mailbox = Lwt_mvar.create_empty () in
  let waiter, wakener = Lwt.task () in
  let handler_lock = Lwt_mutex.create () in
  ( module struct
    module Runner = Impl

    let instance =
      { Impl.task_queue
      ; task_queue_writer
      ; task_state
      ; task_state_lock
      ; task_mailbox
      ; waiter
      ; wakener
      ; handler_lock }
  end
  : Instance )
