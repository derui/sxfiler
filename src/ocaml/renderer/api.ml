module S = Sxfiler_common.Server_state
module type Api_def = Jsonrpc_ocaml.Client.Api_def

(** Root level APIs *)
module Root = struct
  let result_to_state result =
    let json = Yojson.Basic.to_string result |> Js.string in
    let parsed : S.js Js.t = Js._JSON##parse json in
    S.of_js parsed

  (** Get current state *)
  module Get_current_state : Api_def with type params = unit
                                      and type result = S.t
  = struct
    type result = S.t
    type params = unit

    let name = "getCurrentState"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  (** Mapping pane/swapActivePane *)
  module Swap_active_pane : Api_def with type params = unit
                                     and type result = S.t
  = struct
    type result = S.t
    type params = unit

    let name = "swapActivePane"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end
end
