(** {!Statable} can create new State with Lwt. This module is useful to define
    thread-global and thread-safe state in application.
*)
module type Type = sig
  type t

  val empty : unit -> t
end

module type S = sig
  type state

  val get : unit -> state Lwt.t
  val update : state -> unit Lwt.t
  val with_lock : (state -> 'a Lwt.t) -> 'a Lwt.t
end

module Make (T : Type) : S with type state = T.t = struct
  type state = T.t
  type t = state ref

  let t : t = ref @@ T.empty ()
  (* The mutex to synchronize between threads. *)
  let mutex : Lwt_mutex.t = Lwt_mutex.create ()
  let get () = Lwt.return @@ !t

  let update v =
    t := v ;
    Lwt.return_unit

  let with_lock f = Lwt_mutex.with_lock mutex (fun () -> f !t)
end
