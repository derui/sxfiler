open Abbrev

type error =
  | Invalid_color_format of (string * string) list
  | Store_error          of Common_step_theme.Store_theme.error
[@@deriving eq, show]

module Update_theme : sig
  type input = {
    base_theme : D.Common.Not_empty_string.t option;
    color_codes : (D.Common.Not_empty_string.t * D.Common.Not_empty_string.t) list;
  }

  type work_flow = input -> (D.Theme.color_pairs, error) result Lwt.t
  (** workflow to add the theme *)
end

module Get_theme : sig
  type input = unit

  type work_flow = input -> D.Theme.color_pairs Lwt.t
  (** workflow to remove a key binding for action to key map *)
end

type commands =
  | Update_theme of Update_theme.input
  | Get_theme    of Get_theme.input

val update_theme : Common_step_theme.Store_theme.t -> Update_theme.work_flow
(** implementation to add theme *)

val get_theme : Common_step_theme.Get_current_theme.t -> Get_theme.work_flow
(** implementation to list theme *)
