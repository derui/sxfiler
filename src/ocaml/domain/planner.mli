(** This type defines operations in simulation. *)
module Operation : sig
  type t =
    | Append
    | Delete
    | Remained

  val to_int : t -> int
  val of_int : int -> t option
end

(** Plan is difference between current nodes and simulated it. *)
type plan =
  { operation : Operation.t
  ; node : Node.t }

(** [env] is environment on planning. Planner uses it to simulate and apply plan. *)
type env =
  { from : Filer.id
  ; _to : Filer.id }

(** [simulated] is result of simulation via Planner. *)
type simulated =
  { from : plan list
  ; _to : plan list }

(** Identifier of planner. *)
type id = Uuidm.t

(** [t] is model o planner. *)
type t = private
  { id : id
  ; env : env
  ; nodes : Node.t list
  ; simulated : simulated option }

val make : id:id -> env:env -> nodes:Node.t list -> t
(** [make ~id ~env ~nodes] returns new Planner model. New one does not have any result of simulation. *)

val planning : t -> simulated:simulated -> t
(** [planning t ~simulated] make plan with simulated result. *)

val already_simulated : t -> bool
(** [already_simulated t] returns planner already simulation finished or not. *)

(** This signature is repository to handle planner  *)
module type Repository = sig
  val resolve : id -> t option Lwt.t
  (** [resolve id] gets planner having [id]. *)

  val store : t -> unit Lwt.t
  (** [store t] store [t] to repository. *)
end
