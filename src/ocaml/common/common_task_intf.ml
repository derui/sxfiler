(** This module provides modules and interfaces to define task easily. *)
module T = Common_types

(** The simple interface to run task in SxFiler. *)
module type Task = sig
  type t

  val execute: t -> Common_state.t -> T.Task_result.t Lwt.t
end

(** The interface of task instance to run task real.  *)
module type Task_instance = sig
  module Task: Task

  val instance: Task.t
end
