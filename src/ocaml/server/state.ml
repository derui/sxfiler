module State = Sxfiler_server_core.State

(** Application global state. Do not use directly. *)
let root_state = ref State.empty

(* The mutex to synchronize between threads. *)
let mutex : Lwt_mutex.t = Lwt_mutex.create ()

let get_current_state () = Lwt.return !root_state

let with_lock f =
  let%lwt updated_state = Lwt_mutex.with_lock mutex (fun () -> f !root_state) in
  Lwt.return (root_state := updated_state)
