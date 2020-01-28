(** Completer provides simple completion interface via string. *)
module Item_id = Common.Identity.Make (struct
  type t = string [@@deriving eq, show, ord]
end)

(** Item is only type to complete by RPC with any value. *)
module Item = struct
  type t = {
    id : Item_id.t;
    value : string;
  }
  [@@deriving show, eq]

  let make ~id ~value = { id = Item_id.make id; value }
end

(** {!Candidate} defines type of result of completion. *)
module Candidate = struct
  type t = {
    start : int;
    (* 0-origin index of match *)
    length : int;
    (* length of match started from [start] *)
    value : Item.t;
  }
  [@@deriving show, eq]

  let make ~start ~length ~value = { start; length; value }

  let id { value = { id; _ }; _ } = id

  let value { value = { value; _ }; _ } = value
end

type collection = Item.t list [@@deriving show, eq]
(** Type of collection that is used to source of completion. *)

type candidates = Candidate.t list [@@deriving show, eq]
(** Result of completion. *)

module type S = sig
  type t
  (** The type of completer. *)

  val read : input:string -> collection:collection -> t -> candidates
  (** Match [input] from [candidates] with method specified by [match_type].

      This function gives module to stringify a value of type [candidates], so this function can handle any type of
      candidate. *)
end

module type Instance = sig
  module Completer : S

  val this : Completer.t
end
