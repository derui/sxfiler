(** Theme domain provides management theme for style. *)

(* abstract type for color *)

(* base color type *)
module Color : sig
  type t [@@deriving show, eq]

  val of_int : int -> t
  (** make color from int *)

  val to_int : t -> int
  (** get raw color value from this *)
end

(* phantom type applied colors *)
type red = Color.t [@@deriving show, eq]

type green = Color.t [@@deriving show, eq]

type blue = Color.t [@@deriving show, eq]

type alpha = Color.t [@@deriving show, eq]

module Color_code : sig
  type t =
    | RGB  of (red * green * blue)
    | RGBA of (red * green * blue * alpha)
  [@@deriving show, eq]

  val of_string : string -> t option
  (** Get color code from string that is CSS Hex color code. ex) '#fff', '#ffeecc', '#ffaabbff' *)

  val to_string : t -> string
  (** Get CSS Hex color representation from color code *)
end

(* map of color codes *)
module Color_map : (Map.S with type key = Common.Not_empty_string.t [@deriving show, eq])

type t = private {
  name : Common.Not_empty_string.t;
  description : Common.Not_empty_string.t option;
  colors : Color_code.t Color_map.t;
}
[@@deriving show, eq]

val make :
  name:Common.Not_empty_string.t ->
  ?description:Common.Not_empty_string.t ->
  colors:(Common.Not_empty_string.t * Color_code.t) list ->
  unit ->
  t
(** factory function of theme. *)
