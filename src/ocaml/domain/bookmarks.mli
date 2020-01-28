open Sxfiler_core

(** name of the bookmark *)
module Name : sig
  type t [@@deriving show, eq, ord]

  val make : string -> t

  val value : t -> string
end

(** item in bookmarks *)
module Item : sig
  type t = private {
    (* name of the path *)
    name : Name.t;
    (* path of the node *)
    path : Path.t;
  }
  [@@deriving show, eq]
end

type t [@@deriving show]

val empty : t
(** bound empty instance of [t] *)

val insert : name:Name.t -> path:Path.t -> t -> t
(** [insert ~name ~path t] add item having [name] and [path] to [t] *)

val remove : Name.t -> t -> t
(** [remove name t] remove item having [name] from [t] *)

val items : t -> Item.t list
(** [items t] returns instances [Item.t] as list that is not sort. *)

val to_collection : t -> Completer.collection
(** [to_collection t] converts items of [t] to {!Completer.collection} *)
