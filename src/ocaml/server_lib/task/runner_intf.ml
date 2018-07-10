open Sxfiler_server_core

(** module signature of Runner.  *)
module type S = sig
  type t

  (** [add_task task] add task to task queue. *)
  val add_task: t -> (module Intf.Instance) -> unit Lwt.t

  (** [start t ~state] run task accepter and task loop on [t]. *)
  val start: t
    -> state:(module Statable.S with type state = Root_state.t)
    -> unit Lwt.t

  (** [add_task_handler t ~handler] adds [handler] to [t] as handling task result. *)
  val add_task_handler: t
    -> name:string
    -> handler:((module Statable.S with type state = Root_state.t) -> Intf.task_result -> unit Lwt.t)
    -> unit Lwt.t

  (** [remove_task_handler t ~handler] removes [handler] from [t]. *)
  val remove_task_handler: t
    -> name:string
    -> handler:((module Statable.S with type state = Root_state.t) -> Intf.task_result -> unit Lwt.t)
    -> unit Lwt.t

  (** [stop t] will stop task loop and accepter in this module. Wait returned thread [start] if you
      want to handle after runner finished.
  *)
  val stop: t -> unit
end

(** the sigunature for Instance of Runner. *)
module type Instance = sig
  module Runner: S

  val instance: Runner.t
end
