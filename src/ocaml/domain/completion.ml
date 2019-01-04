(** Completion defines value object to complete something.  *)

(** Item is only type to complete by RPC with any value. *)
module Item = struct
  type t =
    { id : string
    ; value : string }
end

(** {!Candidate} defines type of result of completion. *)
module Candidate = struct
  type 'a base =
    { start : int
    ; length : int
    ; value : 'a }

  type t = Item.t base

  (* shortcut functions for {!t} *)

  (** [id t] gets the identifier of [t]. *)
  let id t = t.value.Item.id

  (** [value t] gets the value of [t]. *)
  let value t = t.value.Item.value
end

(** Type of collection that is used to source of completion. *)
type collection = Item.t list

(** Result of completion. *)
type result = Candidate.t list

(** Repository interface *)
module type Repository = sig
  val store : collection -> unit Lwt.t
  (** [store collection] saves collection to some space. *)

  val resolve : unit -> collection Lwt.t
  (** [resolve ()] returns collection that is stored now.  *)
end