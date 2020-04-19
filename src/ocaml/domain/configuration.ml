(** Configuration domain has all configurations of application. *)

type t = {
  default_sort_order : Types.Sort_type.t;
  confirmation_when_delete : bool;
  max_history_num : Common.Positive_number.t;
  current_theme : string;
}
[@@deriving show, eq]

let default =
  {
    default_sort_order = Types.Sort_type.Name;
    confirmation_when_delete = true;
    max_history_num = Common.Positive_number.make 100 |> Option.get;
    current_theme = "default";
  }

let default_sort_order sort_order t = { t with default_sort_order = sort_order }

let confirmation_when_delete confirmation t = { t with confirmation_when_delete = confirmation }

let max_history_num max_history_num t = { t with max_history_num }

let current_theme current_theme t = { t with current_theme }
