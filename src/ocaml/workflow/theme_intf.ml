open Abbrev

type error =
  | Invalid_color_format of (string * string) list
  | Store_error          of Common_step_theme.error
[@@deriving eq, show]

module Update_theme = struct
  type input = {
    base_theme : D.Common.Not_empty_string.t option;
    color_codes : (D.Common.Not_empty_string.t * D.Common.Not_empty_string.t) list;
  }

  type output = (D.Theme.color_pairs, error) result
end

module Get_theme = struct
  type input = unit

  type output = D.Theme.color_pairs
  (** workflow to remove a key binding for action to key map *)
end

type commands =
  | Update_theme of Update_theme.input
  | Get_theme    of Get_theme.input
