module S = Sxfiler_common.State
module type Api_def = Jsonrpc_ocaml.Client.Api_def

module type Api_base = Api_def with type result = S.t

(** Get current state *)
module Current_state : Api_base with type params = unit
= struct
  type result = S.t
  type params = unit

  let name = "/current-state"
  let params_to_json _ = None
  let result_of_json result =
    let json = Yojson.Basic.to_string result |> Js.string in
    let parsed : S.js Js.t = Js._JSON##parse json in
    S.of_js parsed
end
