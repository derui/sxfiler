open Sxfiler_domain

type unsubscribe = unit -> unit Lwt.t
type subscriber = Task.t -> unit Lwt.t

(** module signature of Runner. *)
module type S = sig
  type t

  val add_task : t -> task:Task.t -> unit Lwt.t
  (** [add_task ~task t] add the task to task queue. *)

  val stop_task : t -> task:Task_types.id -> unit Lwt.t
  (** [stop_task ~task t] stop the task from task queue *)

  val subscribe : t -> f:subscriber -> unsubscribe Lwt.t
  (** [subscribe ~f t] register a function as subscriber to subscribe event to finish task. if want
      to unsubscribe, execute the function returned when registered *)

  val start : t -> unit Lwt.t
  (** [start t ~state] run task accepter and task loop on [t]. *)

  val stop : t -> unit
  (** [stop t] will stop task loop and accepter in this module. Wait returned thread [start] if you
      want to handle after runner finished. *)
end

(** the sigunature for Instance of Runner. *)
module type Instance = sig
  module Runner : S

  val instance : Runner.t
end
