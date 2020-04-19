(** Completion defines value object to complete something. *)

module Item_id : Common.Identity.S with type value := string

(** Item is only type to complete by RPC with any value. *)
module Item : sig
  type t = private {
    id : Item_id.t;
    value : string;
  }
  [@@deriving show, eq]

  val make : id:string -> value:string -> t
  (** [make ~id ~value] gets a new instance of [t] *)
end

(** {!Candidate} defines type of result of completion. *)
module Candidate : sig
  type t = private {
    start : int;
    (* 0-origin index of matched phrase *)
    length : int;
    (* length of match started from [start] *)
    value : Item.t;
  }
  [@@deriving show, eq]

  val make : start:int -> length:int -> value:Item.t -> t
  (** [make ~start ~length ~value] gets a new instance of [t] *)

  (* shortcut functions for {!t} *)

  val id : t -> Item_id.t
  (** [id t] gets the identifier of [t]. *)

  val value : t -> string
  (** [value t] gets the value of [t]. *)
end

type collection = Item.t list [@@deriving show, eq]
(** Type of collection that is used to source of completion. *)

type candidates = Candidate.t list [@@deriving show, eq]
(** Result of completion. *)

(** Signature for completer *)
module type S = sig
  type t
  (** The type of completer. *)

  val read : input:string -> collection:collection -> t -> candidates
  (** Match [input] from [candidates] with method specified by [match_type].

      This function gives module to stringify a value of type [candidates], so this function can handle any type of
      candidate. *)
end

(** Signature for the completer module *)
module type Instance = sig
  module Completer : S

  val this : Completer.t
end
