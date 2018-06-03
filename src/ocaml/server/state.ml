module C = Sxfiler_common

(** Application global state. Do not use directly. *)
let root_state = ref C.Server_state.empty

(* The mutex to synchronize between threads. *)
let mutex : Lwt_mutex.t = Lwt_mutex.create ()

let get_current_state = !root_state

let with_lock t f =
  let%lwt updated_state = Lwt_mutex.with_lock mutex (fun () -> f !root_state) in
  Lwt.return (root_state := updated_state)
