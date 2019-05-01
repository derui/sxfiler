module D = Sxfiler_domain

type t =
  { default_sort_order : Types.Sort_type.t [@key "defaultSortOrder"]
  ; key_map_file : string [@key "keyMapFile"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let of_domain t =
  { default_sort_order = Types.Sort_type.of_domain t.D.Configuration.default_sort_order
  ; key_map_file = Sxfiler_core.Path.to_string t.key_map_file }

let to_domain t =
  { D.Configuration.default_sort_order = Types.Sort_type.to_domain t.default_sort_order
  ; key_map_file = Sxfiler_core.Path.of_string t.key_map_file }
