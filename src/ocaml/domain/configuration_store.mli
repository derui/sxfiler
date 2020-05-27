(** Configuration store is the store of configuration for frontend.

    This store is as key-value stores that value in it as json because why these are for frontend. OCaml side does not
    use this module in business logic, because OCaml does not rely on JSON. So, this module do not handle any input
    error and value restriction. However if it need, DO it in frontend. *)

module Key : sig
  type t [@@deriving show, eq]

  val from_list : string list -> t option
  (** create key from list of string. Return [None] if the list is empty. *)

  val to_list : t -> string list
  (** get original key list from [t] *)
end

type t [@@deriving show, eq]

val empty : t
(** [empty] bound empty configuration store values of [t] *)

val put : key:Key.t -> value:Yojson.Basic.t -> t -> t
(** [put ~key ~value t] put the [value] to [key] *)

val keys : t -> Key.t list
(** [keys t] get all keys stored in [t] *)

val get : key:Key.t -> t -> Yojson.Basic.t option
(** [get ~key t] get the value from key. All values are JSON in store. *)

val to_list : t -> (Key.t * Yojson.Basic.t) list
(** [to_list t] list stored values in [t] *)

val of_json : Yojson.Basic.t -> t
(** [of_json json] restore store from json *)
