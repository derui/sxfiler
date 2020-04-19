(** {!Statable} can create new State with Lwt. This module is useful to define thread-global and thread-safe state in
    application. *)

open Sxfiler_core

module type Type = sig
  type t

  val empty : unit -> t
end

module type S = sig
  type state

  val get : unit -> state Lwt.t

  val update : state -> unit Lwt.t
end

module Make (T : Type) : S with type state = T.t = struct
  type state = T.t

  type t = state ref

  let mutex = Lwt_mutex.create ()

  let t : t = ref & T.empty ()

  (* The mutex to synchronize between threads. *)
  let get () = Lwt_mutex.with_lock mutex (fun () -> Lwt.return !t)

  let update v =
    Lwt_mutex.with_lock mutex (fun () ->
        t := v;
        Lwt.return_unit)
end
