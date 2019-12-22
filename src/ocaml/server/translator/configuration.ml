module D = Sxfiler_domain
module G = Sxfiler_server_generated

type t = G.Configuration.Configuration.t
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let of_domain (t : D.Configuration.t) =
  Types.Sort_type.of_domain t.D.Configuration.default_sort_order

let to_domain (t : t) = { D.Configuration.default_sort_order = Types.Sort_type.to_domain t }
