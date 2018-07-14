(** The module defines tag to specify store. *)

(** The type of tag *)
type ('a, 'b) def

(** [def ~name] gets definition of tag. You should specify phantom type for {!def}. *)
val def: name:string -> store:(module Store_intf.S with type t = 'a and type message = 'b) -> ('a, 'b) def

(** [name def] gets the name of [def] *)
val name: ('a, 'b) def -> string

(** [store def] gets the store module of [def] *)
val store: ('a, 'b) def -> (module Store_intf.S with type t = 'a and type message = 'b)
