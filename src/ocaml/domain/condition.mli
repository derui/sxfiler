(** Condition defines condition of context such as starting completion, inputting on command. *)

(** abstract type for Condition *)
type t

val equal : t -> t -> bool
(** [equal v1 v2] returns equal v1 and v2 or not*)

val empty : t
(** Get a empty condition {!t}. Condition is immutable. *)

val of_list : string list -> t
(** [of_list contexts] returns new condition switched on given contexts. *)

val to_list : t -> string list
(** [to_list t] returns list of context that is contains only switched on. *)

val enable : t -> context:string -> t
(** [enable t ~context] returns new condition context enabled. *)

val disable : t -> context:string -> t
(** [disable t ~context] returns new condition context disabled. *)

val subset : current:t -> parts:t -> bool
(** [subset ~current ~parts] returns what [parts] is subset of [current] or not. This function is useful to check
    if current context was fulfilled condition specified by [parts].
*)

module type Repository = sig
  val store : t -> unit Lwt.t
  (** [store t] store [t] to current condition. *)

  val resolve : unit -> t Lwt.t
  (** [resolve ()] returns condition as singleton instance. *)
end
