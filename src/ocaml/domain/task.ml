type id = string [@@deriving show, eq]

type interaction =
  | Yes_no of bool
  | String of string
  | Int of int

type status =
  | Pending
  | Running
  | Wait_interaction
[@@deriving show, eq]

type context = {task_id : id}

(** {!S} is signature to do the task. *)
module type Executor = sig
  val apply_interaction : [`No_interaction | `Apply of interaction -> unit Lwt.t]
  (** [apply_interaction interaction] applies an interaction to the executor.
      The Executor is able to do not define anything for interaction.
  *)

  val execute : context -> unit Lwt.t
  (** [execute ()] runs the task body. *)
end

(** [!t] is . *)
type t =
  { id : id
  ; executor : (module Executor) [@printer fun _ _ -> ()]
  ; status : status }
[@@deriving show, fields]

(** [make ~id ~operation ~nodes] makes new plan [t] instance from. *)
let make ~id ~executor ~status = {id; executor; status}

(** [execute t] execute the plan [t] with {!Executor}. *)
let execute t =
  let module E = (val t.executor) in
  E.execute {task_id = t.id}

(** [apply_interaction ~interaction t] apply an interaction to the task *)
let apply_interaction ~interaction t =
  let module E = (val t.executor) in
  match E.apply_interaction with `No_interaction -> Lwt.return_unit | `Apply f -> f interaction

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
    let create = make ~id:(G.generate ()) ~status:Pending
  end
end
