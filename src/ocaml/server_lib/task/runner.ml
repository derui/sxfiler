(** This module will run tasks with task queue.  *)

(** only entry point to add task to task queue in this module. *)
type thread_state = [ `Accepted of (module Intf.Instance) | `Rejected ]
let task_mailbox : thread_state Lwt_mvar.t = Lwt_mvar.create_empty ()

module Task_state = Map.Make(struct
    type t = Uuidm.t
    let compare = Uuidm.compare
  end)

let task_queue_lock = Lwt_mutex.create ()
let task_queue_signal = Lwt_condition.create ()
let task_state_lock = Lwt_mutex.create ()
let task_state : unit Task_state.t ref = ref Task_state.empty

(** Forever loop to accept task. Running this function must be in other thread of worker thread. *)
let rec accept_task_loop () =
  let open Lwt in
  Lwt_mvar.take task_mailbox
  >>= fun task -> Lwt_condition.broadcast task_queue_signal task; Lwt.return task
  >>= function
  | `Accepted _ -> accept_task_loop ()
  | `Rejected -> return_unit

let remove_state id result =
  Lwt_mutex.with_lock task_state_lock (fun () ->
      task_state := Task_state.remove id !task_state;
      Lwt.return result
    )

(** Forever loop to run task. *)
let rec run_task_loop s task_handler () =
  let open Lwt in
  let module C = Sxfiler_server_core in
  let module S = (val s : C.Statable.S with type state = C.Root_state.t) in
  Lwt_condition.wait ~mutex:task_queue_lock task_queue_signal
  >>= function
  | `Rejected -> return_unit
  | `Accepted task -> begin
      Lwt_mutex.with_lock task_state_lock (fun () ->
          let id = Uuidm.v4_gen (Random.get_state ()) () in

          task_state := Task_state.add id () !task_state;

          let module Current_task = (val task : Intf.Instance) in
          Lwt.async (fun () ->
              let%lwt result = try%lwt
                  let%lwt state' = S.get () in
                  let%lwt ret = Current_task.Task.apply state' Current_task.instance (module Current_task.Action) in
                  remove_state id ret
                with _ ->
                  remove_state id (`Failed "Failed task with unhandled exception")
              in
              task_handler s result
            );
          Lwt.return_unit
        )
      >>= run_task_loop s task_handler
    end

(** [add_task task] add [task] to mailbox of task accepter. *)
let add_task task = Lwt_mvar.put task_mailbox (`Accepted task)

(** [start ~state ~task_handler] start asynchronous threads that are task accepter and task worker.
    Passing [state] is used with Task to pass current state.
    [task_handler] will handle result of task.
    Use {!Lwt.wakeup} with result of this function.
*)
let start ~state ~task_handler =
  let module C = Sxfiler_server_core in
  let accepter = accept_task_loop () in
  let worker = run_task_loop state task_handler () in

  let waiter, wakener = Lwt.task () in
  let open Lwt in
  let stopper = waiter >>= fun () ->
    (* Cancel all async threads. *)
    Lwt_mvar.put task_mailbox `Rejected >>= fun () ->
    Lwt.join [accepter;worker] >>= Lwt.return
  in
  Lwt.async (fun () -> accepter);
  Lwt.async (fun () -> worker);
  (wakener, stopper)
