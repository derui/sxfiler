(** This module will run tasks with task queue.  *)

(** only entry point to add task to task queue in this module. *)
let task_accepter = Lwt_mvar.create_empty ()

module Task_state = Map.Make(struct
    type t = Uuidm.t
    let compare = Uuidm.compare
  end)

let task_queue_lock = Lwt_mutex.create ()
let task_queue_signal = Lwt_condition.create ()
let task_state_lock = Lwt_mutex.create ()
let task_state : unit Task_state.t ref = ref Task_state.empty

(** Forever loop to accept task. Running this function must be in other thread of worker thread. *)
let rec accept_task () =
  let open Lwt in
  Lwt_mvar.take task_accepter
  >>= fun task -> Lwt_condition.broadcast task_queue_signal task |> Lwt.return
  >>= accept_task

let handle_task_result id result =
  Lwt_mutex.with_lock task_state_lock (fun () ->
      task_state := Task_state.remove id !task_state;
      Lwt.return result
    )

(** Forever loop to run task. *)
let rec run_task () =
  let open Lwt in
  Lwt_condition.wait ~mutex:task_queue_lock task_queue_signal
  >>= fun task ->
  Lwt_mutex.with_lock task_state_lock (fun () ->
      let id = Uuidm.v4_gen (Random.get_state ()) () in

      task_state := Task_state.add id () !task_state;

      let module Current_task = (val task : Task_intf.Instance) in
      Lwt.async (fun () ->
          try%lwt
            let%lwt state = Lwt.wrap State.get_current_state in
            let%lwt ret = Current_task.Task.apply state Current_task.instance (module Current_task.Action) in
            handle_task_result id ret
          with _ ->
            handle_task_result id (`Failed "Failed task with unhandled exception")
        );
      Lwt.return_unit
    )
  >>= run_task

(** [start ()] start asynchronous threads that are task accepter and task worker.
    Use {!Lwt.wakeup} with result of this function.
*)
let start () =
  let accepter = accept_task () in
  let worker = run_task () in

  let waiter, wakener = Lwt.task () in
  let open Lwt in
  let stopper = waiter >>= fun () ->
    (* Cancel all async threads. *)
    Lwt.cancel accepter;
    Lwt.cancel worker;
    Lwt.return_unit
  in
  Lwt.async (fun () -> accepter);
  Lwt.async (fun () -> worker);
  (wakener, stopper)
