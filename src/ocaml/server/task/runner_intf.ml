(** module signature of Runner.  *)
module type S = sig
  type t

  val add_task : t -> (module Task.Instance) -> unit Lwt.t
  (** [add_task task] add task to task queue. *)

  val start : t -> unit Lwt.t
  (** [start t ~state] run task accepter and task loop on [t]. *)

  val stop : t -> unit
  (** [stop t] will stop task loop and accepter in this module. Wait returned thread [start] if you
      want to handle after runner finished.
  *)
end

(** the sigunature for Instance of Runner. *)
module type Instance = sig
  module Runner : S

  val instance : Runner.t
end