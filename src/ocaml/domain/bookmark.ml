open Sxfiler_core

type id = Uuidm.t [@@deriving show, eq]

type t = {
  (* path of the node *)
  id : id;
  path : Path.t;
  order : int;
}
[@@deriving show, eq]

let make ~id ~path ~order = { id; path; order }

(** [have_same_id v1 v2] return having same id or not between v1 and v2. *)
let have_same_id v1 v2 = Uuidm.equal v1.id v2.id
