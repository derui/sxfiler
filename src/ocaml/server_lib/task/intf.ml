(** Intf defines module type to make task. *)

open Sxfiler_types
module State = Sxfiler_server_core.Root_state

type plan = Tree_snapshot.t * Tree_snapshot.t

type task_result = [
  | `Update_workspace of string * Tree_snapshot.t
  | `Failed of string
]

type 'a task = ()

module A = Sxfiler_server_action.Action_intf

module type S = sig
  type params

  (** [plan] get result that task applied virtually. If [`No_plan] passed,
      task runner do not run plan and run [apply] immediately.
  *)
  val plan: [ `No_plan
            | `Having_plan of (State.t -> params -> (module A.Instance) -> plan Lwt.t)]

  (** [apply state] get result of task. All task should return one of type result. *)
  val apply: State.t -> params -> (module A.Instance) -> task_result Lwt.t
end

module type Instance = sig
  type t
  module Task: S with type params = t
  module Action: A.Instance

  val instance: t
end

(** [make_instance params (module Task)] make a instance of Task with parameters [params]. *)
let make_instance (type s) params
    (action: (module A.Instance))
    (task : (module S with type params = s)) =
  let module Task = (val task : S with type params = s) in
  (module struct
    type t = s
    module Task = Task
    module Action = (val action : A.Instance)

    let instance = params
  end : Instance)