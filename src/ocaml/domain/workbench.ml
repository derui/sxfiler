(** Identifier of plan. *)
type id = Uuidm.t

(** [env] holds environment is based on plan. *)
type env =
  { source : Filer.t
  ; dest : Filer.t
  ; nodes : Node.t list }

(** [corrections] contains corrected node in workbench. *)
type corrections = Node.t list

(** type of plan *)
type t =
  { id : id
  ; env : env
  ; corrections : corrections }

(** [make ~id ~env] makes instance of plan *)
let make ~id ~env ~corrections = {id; env; corrections}

(** Signature of repository for [t] *)
module type Repository = sig
  val resolve : id -> t option Lwt.t
  (** [resolve id] returns the plan having [id] if exists. *)

  val store : t -> unit Lwt.t
  (** [store t] saves [t] to repository. *)

  val remove : t -> unit Lwt.t
  (** [remove t] removes the plan [t] from repository. Do not use [t] after removed. *)
end

(** Signature of factory for [t] *)
module type Factory = sig
  val make : env -> t
  (** [make env] returns new instance of {!t} *)
end
