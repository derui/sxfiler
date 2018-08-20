(** Condition defines condition of context such as starting completion, inputting on command. *)

(** abstract type for Condition *)
type t

(** [equal v1 v2] returns equal v1 and v2 or not*)
val equal: t -> t -> bool

(** Get a empty condition {!t}. Condition is immutable. *)
val empty : t

(** [of_list contexts] returns new condition switched on given contexts. *)
val of_list: string list -> t

(** [to_list t] returns list of context that is contains only switched on. *)
val to_list: t -> string list

(** [enable t ~context] returns new condition context enabled. *)
val enable: t -> context:string -> t

(** [disable t ~context] returns new condition context disabled. *)
val disable: t -> context:string -> t

(** [subset ~current ~parts] returns what [parts] is subset of [current] or not. This function is useful to check
    if current context was fulfilled condition specified by [parts].
*)
val subset : current:t -> parts:t -> bool

module type Repository = sig
  (** [store t] store [t] to current condition. *)
  val store: t -> unit Lwt.t

  (** [resolve ()] returns condition as singleton instance. *)
  val resolve: unit -> t Lwt.t
end
