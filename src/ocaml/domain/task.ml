open Task_types

(** {!S} is signature to do the task. *)
module type Executor = sig
  val apply_interaction : [ `No_interaction | `Apply of Task_interaction.Reply.typ -> unit Lwt.t ]
  (** [apply_interaction interaction] applies an interaction to the executor. The Executor is able
      to do not define anything for interaction. *)

  val execute : Context.t -> unit Lwt.t
  (** [execute ()] runs the task body. *)
end

type t = {
  id : id;
  executor : (module Executor); [@printer fun _ _ -> ()]
}
[@@deriving show]
(** [!t] is . *)

(** [make ~id ~operation] makes new plan [t] instance from. *)
let make ~id ~executor = { id; executor }

(** [execute t] execute the plan [t] with {!Executor}. *)
let execute t =
  let module E = (val t.executor) in
  E.execute { task_id = t.id }

(** [apply_interaction ~interaction t] apply an interaction to the task *)
let apply_interaction ~reply t =
  let module E = (val t.executor) in
  match E.apply_interaction with `No_interaction -> Lwt.return_unit | `Apply f -> f reply

let have_same_id t1 t2 = equal_id t1.id t2.id

module type Repository = sig
  val store : t -> unit Lwt.t
  (** [store t] saves instance [t] *)

  val resolve : id -> t option Lwt.t
  (** [resolve id] get the instance having [id]. *)

  val remove : t -> unit Lwt.t
  (** [remove t] removes [t] from repository. *)
end

(** Factory interface for [type: t] *)
module Factory = struct
  module type S = sig
    val create : executor:(module Executor) -> t
  end

  module Make (G : Id_generator_intf.Gen_random with type id = id) : S = struct
    let create = make ~id:(G.generate ())
  end
end
