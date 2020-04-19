(** Condition defines condition of context such as starting completion, inputting on command. *)

type t [@@deriving show, eq, ord]
(** abstract type for Condition *)

val empty : t
(** Get a empty condition {!t}. Condition is immutable. *)

val of_list : string list -> t
(** [of_list contexts] returns new condition switched on given contexts. *)

val to_list : t -> string list
(** [to_list t] returns list of context that is contains only switched on. *)

val enable : context:string -> t -> t
(** [enable ~context t] returns new condition context enabled. *)

val disable : context:string -> t -> t
(** [disable ~context t] returns new condition context disabled. *)

val subset : parts:t -> t -> bool
(** [subset ~parts current] returns what [parts] is subset of [current] or not. This function is useful to check if
    current context was fulfilled condition specified by [parts]. *)
