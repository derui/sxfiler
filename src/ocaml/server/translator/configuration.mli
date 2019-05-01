(** the type that is JSON friendly for {!Sxfiler_domain.Configuration.t} *)
type t =
  { default_sort_order : Types.Sort_type.t [@key "defaultSortOrder"]
  ; key_map_file : string [@key "keyMapFile"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Configuration.t
