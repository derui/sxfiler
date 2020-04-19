open Sxfiler_core
module D = Sxfiler_domain
module G = Sxfiler_generated

type t = G.Configuration.Configuration.t

type error =
  | Max_history_num    of int
  | Default_sort_order of string
[@@deriving eq, show]

let of_domain (t : D.Configuration.t) =
  {
    G.Configuration.Configuration.default_sort_order = Types.Sort_type.of_domain t.D.Configuration.default_sort_order;
    confirmation_when_delete = t.confirmation_when_delete;
    max_history_num = D.Common.Positive_number.value t.max_history_num;
    current_theme = t.current_theme;
  }

let to_domain (t : t) =
  let open D.Configuration in
  let open Result.Infix in
  let* num =
    D.Common.Positive_number.make t.max_history_num |> Option.to_result ~none:(Max_history_num t.max_history_num)
  in
  let* sort_order =
    Types.Sort_type.to_domain t.default_sort_order |> Result.map_error (fun v -> Default_sort_order v)
  in
  default |> default_sort_order sort_order
  |> confirmation_when_delete t.confirmation_when_delete
  |> max_history_num num |> current_theme t.current_theme |> Result.ok
