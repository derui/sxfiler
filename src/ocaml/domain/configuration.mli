(** Configuration domain has all configurations of application. *)

type t = private {
  default_sort_order : Types.Sort_type.t;
  confirmation_when_delete : bool;
  max_history_num : Common.Positive_number.t;
  current_theme : string;
}
[@@deriving show, eq]

val default : t
(** [default] bound default values of [t] *)

val default_sort_order : Types.Sort_type.t -> t -> t

val confirmation_when_delete : bool -> t -> t

val max_history_num : Common.Positive_number.t -> t -> t

val current_theme : string -> t -> t
