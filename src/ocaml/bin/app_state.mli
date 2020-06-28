(** Types that are allowed to use in this application state. *)
type _ typ =
  | Int : int typ
  | String : string typ
  | Bool : bool typ
  | Pair   : ('a typ * 'b typ) -> ('a * 'b) typ
  | List   : 'a typ -> 'a list typ

val ( %* ) : 'a typ -> 'b typ -> ('a * 'b) typ
(** Helper function to make pair type *)

type state
(** type for state. *)

type app_state
(** All statement for application. *)

val create : unit -> app_state
(** Create new application state container *)

val read_state : typ:'a typ -> key:string -> app_state -> 'a option
(** read a state from application state *)

val write_state : typ:'a typ -> key:string -> value:'a -> app_state -> unit
(** write a state to application state *)

val to_json : app_state -> Yojson.Basic.t
(** convert application state to JSON. *)

val of_json : Yojson.Basic.t -> app_state
(** load application state from JSON *)
