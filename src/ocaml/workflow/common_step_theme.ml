open Abbrev

module Store_theme = struct
  type base_theme = D.Common.Not_empty_string.t

  type error = Unknown_error of string [@@deriving show, eq]

  type t =
    (D.Common.Not_empty_string.t * D.Theme.Color_code.t) list ->
    base_theme option ->
    (D.Theme.color_pairs, error) result Lwt.t
  (** store theme *)
end

module Get_current_theme = struct
  type t = unit -> D.Theme.color_pairs Lwt.t
  (** Get current theme *)
end
