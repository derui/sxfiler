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
let rec accept_task_loop switch () =
  let open Lwt in
  if Lwt_switch.is_on switch then
    Lwt_mvar.take task_mailbox
    >>= fun task -> Lwt_condition.broadcast task_queue_signal task; Lwt.return task
    >>= function
    | `Accepted _ -> accept_task_loop switch ()
    | `Rejected -> return_unit
  else
    return_unit

let handle_task_result id result =
  Lwt_mutex.with_lock task_state_lock (fun () ->
      task_state := Task_state.remove id !task_state;
      Lwt.return result
    )

(** Forever loop to run task. *)
let rec run_task_loop get_state switch () =
  let open Lwt in
  if not @@ Lwt_switch.is_on switch then return_unit
  else
    Lwt_condition.wait ~mutex:task_queue_lock task_queue_signal
    >>= function
    | `Rejected -> return_unit
    | `Accepted task -> begin
        Lwt_mutex.with_lock task_state_lock (fun () ->
            let id = Uuidm.v4_gen (Random.get_state ()) () in

            task_state := Task_state.add id () !task_state;

            let module Current_task = (val task : Intf.Instance) in
            Lwt.async (fun () ->
                try%lwt
                  let%lwt state = get_state () in
                  let%lwt ret = Current_task.Task.apply state Current_task.instance (module Current_task.Action) in
                  handle_task_result id ret
                with _ ->
                  handle_task_result id (`Failed "Failed task with unhandled exception")
              );
            Lwt.return_unit
          )
        >>= run_task_loop get_state switch
      end

(** [add_task task] add [task] to mailbox of task accepter. *)
let add_task task = Lwt_mvar.put task_mailbox (`Accepted task)

(** [start get_state] start asynchronous threads that are task accepter and task worker.
    Passing [get_state] is used with Task to pass current state.
    Use {!Lwt.wakeup} with result of this function.
*)
let start get_state =
  let accepter_switch = Lwt_switch.create ()
  and worker_switch = Lwt_switch.create () in
  let accepter = accept_task_loop accepter_switch () in
  let worker = run_task_loop get_state worker_switch () in

  let waiter, wakener = Lwt.task () in
  let open Lwt in
  let stopper = waiter >>= fun () ->
    (* Cancel all async threads. *)
    Lwt.join [Lwt_switch.turn_off accepter_switch; Lwt_switch.turn_off worker_switch] >>= fun () ->
    Lwt_mvar.put task_mailbox `Rejected >>= fun () ->
    Lwt.join [accepter;worker] >>= Lwt.return
  in
  Lwt.async (fun () -> accepter);
  Lwt.async (fun () -> worker);
  (wakener, stopper)
