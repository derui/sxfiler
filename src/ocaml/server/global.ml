module State = Sxfiler_server_core.Root_state

module Root = struct

  (** Application global state. Do not use directly. *)
  let t = ref Root_state.empty

  (* The mutex to synchronize between threads. *)
  let mutex : Lwt_mutex.t = Lwt_mutex.create ()

  let get () = Lwt.return !t
  let update v = t := v

  let with_lock f = Lwt_mutex.with_lock mutex (fun () -> f !t)
end

module Completion = struct
  type t = Sxfiler_server_completion.Completer.t

  (** Application global state. Do not use directly. *)
  let t : t option ref = ref None

  (* The mutex to synchronize between threads. *)
  let mutex : Lwt_mutex.t = Lwt_mutex.create ()

  let get () = Lwt.return !t
  let update v = t := v

  let with_lock f = Lwt_mutex.with_lock mutex (fun () -> f !t)
end
