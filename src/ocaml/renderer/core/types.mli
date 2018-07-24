(** {!Mode} defines type that is current mode of view. *)
module Mode : sig
  type t =
    | Command
    | Completion
    | File_tree
end

(** Condition defines condition of context such as starting completion, inputting on command. *)
module Condition : sig

  type context =
    | On_file_tree
    | On_completing

  (** abstract type for Condition *)
  type t

  (** JSON representation. *)
  class type js = object
    method onCompleting : bool Js.t Js.optdef Js.readonly_prop
    method onFileTree : bool Js.t Js.optdef Js.readonly_prop
  end

  (** [equal v1 v2] returns equal v1 and v2 or not*)
  val equal: t -> t -> bool

  (** Get a empty condition {!t}. Condition is immutable. *)
  val empty : t

  (** [of_list contexts] returns new condition switched on given contexts. *)
  val of_list: context list -> t

  (** [to_js t] convert condition to JSON representation {!js} *)
  val to_js : t -> js Js.t

  (** [of_js js] convert JSON representation with {!js} to condition {!t} *)
  val of_js : js Js.t -> t

  (** [enable t ~context] returns new condition context enabled. *)
  val enable: t -> context:context -> t

  (** [disable t ~context] returns new condition context disabled. *)
  val disable: t -> context:context -> t

  (** [subset ~current ~parts] returns what [parts] is subset of [current] or not. This function is useful to check
      if current context was fulfilled condition specified by [parts].
  *)
  val subset : current:t -> parts:t -> bool
end
