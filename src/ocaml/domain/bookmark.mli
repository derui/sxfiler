open Sxfiler_core

type id = Uuidm.t [@@deriving show, eq]

type t = private {
  (* path of the node *)
  id : id;
  path : Path.t;
  order : int;
}
[@@deriving show, eq]

val make : id:id -> path:Path.t -> order:int -> t
(** [make ~id ~path ~order] makes new instance *)

val have_same_id : t -> t -> bool
(** [have_same_id v1 v2] return having same id or not between v1 and v2. *)
