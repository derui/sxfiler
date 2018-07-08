

module type S = sig
  val expose: Jsonrpc_ocaml_yojson.Server.t -> Jsonrpc_ocaml_yojson.Server.t
end
