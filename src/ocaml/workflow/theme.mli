open Abbrev

type error =
  | Invalid_color_format of (string * string) list
  | Store_error          of Common_step_theme.Store_theme.error
  | Remove_error         of Common_step_theme.Remove_theme.error
[@@deriving eq, show]

module Add_theme : sig
  type input = {
    name : D.Common.Not_empty_string.t;
    description : D.Common.Not_empty_string.t option;
    color_codes : (D.Common.Not_empty_string.t * D.Common.Not_empty_string.t) list;
  }

  type work_flow = input -> (D.Theme.t, error) result Lwt.t
  (** workflow to add the theme *)
end

module Remove_theme : sig
  type input = { name : D.Common.Not_empty_string.t }

  type work_flow = input -> (unit, error) result Lwt.t
  (** workflow to remove a key binding for action to key map *)
end

module List_theme : sig
  type input = unit

  type work_flow = input -> D.Theme.t list Lwt.t
  (** workflow to remove a key binding for action to key map *)
end

type commands =
  | Add_theme    of Add_theme.input
  | Remove_theme of Remove_theme.input
  | List_theme   of List_theme.input

val add_theme : Common_step_theme.Store_theme.t -> Add_theme.work_flow
(** implementation to add theme *)

val remove_theme : Common_step_theme.List_theme.t -> Common_step_theme.Remove_theme.t -> Remove_theme.work_flow
(** implementation to remove theme *)

val list_theme : Common_step_theme.List_theme.t -> List_theme.work_flow
(** implementation to list theme *)
