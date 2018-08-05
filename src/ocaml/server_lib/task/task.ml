(** Intf defines module type to make task. *)
module type S = sig
  type t

  (** [run ()] execute the task. All task is as thread, so can not return anything. *)
  val run: t -> unit Lwt.t
end

module type Instance = sig
  module Task: S

  val this: Task.t
end
