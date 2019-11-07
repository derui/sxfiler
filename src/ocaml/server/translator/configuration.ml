module D = Sxfiler_domain

type t = { default_sort_order : Types.Sort_type.t [@key "defaultSortOrder"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let of_domain t =
  { default_sort_order = Types.Sort_type.of_domain t.D.Configuration.default_sort_order }

let to_domain t =
  { D.Configuration.default_sort_order = Types.Sort_type.to_domain t.default_sort_order }
