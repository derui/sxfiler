(** Configuration domain has all configurations of application. *)
open Sxfiler_core

type t =
  { default_sort_order : Types.Sort_type.t
  ; key_map_file : Path.t }
[@@deriving show, eq]

let default =
  {default_sort_order = Types.Sort_type.Name; key_map_file = Path.(of_string "./keymap.json")}

module type Repository = sig
  val resolve : unit -> t Lwt.t
  (** [resolve ()] returns configuration. Configuration should be singleton. *)

  val store : t -> unit Lwt.t
  (** [store t] saves the [t] as singleton *)
end
