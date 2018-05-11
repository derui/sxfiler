module C = Sxfiler_common
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

(** APIs for config  *)
module Config = struct
  let result_to_config result =
    let json = Yojson.Basic.to_string result |> Js.string in
    let parsed : C.Config.js Js.t = Js._JSON##parse json in
    C.Config.of_js parsed

  module Current : Api_def with type params = unit
                            and type result = C.Config.t
  = struct
    type result = C.Config.t
    type params = unit

    let name = "config/current"
    let params_to_json _ = None
    let result_of_json = result_to_config
  end

  module Load : Api_def with type params = unit
                            and type result = C.Config.t
  = struct
    type result = C.Config.t
    type params = unit

    let name = "config/load"
    let params_to_json _ = None
    let result_of_json = result_to_config
  end
end

(** APIs for config  *)
module Pane = struct
  let result_to_state result =
    let json = Yojson.Basic.to_string result |> Js.string in
    let parsed : S.js Js.t = Js._JSON##parse json in
    S.of_js parsed

  module Focus : Api_def with type params = C.Types.file_id
                          and type result = S.t
  = struct
    type result = S.t
    type params = C.Types.file_id

    let name = "pane/focus"
    let params_to_json id =
      match id with
      | None -> None
      | Some id -> Some (`List ([`String id]))
    let result_of_json = result_to_state
  end

  module Toggle_mark : Api_def with type params = C.Types.file_id
                          and type result = S.t
  = struct
    type result = S.t
    type params = C.Types.file_id

    let name = "pane/toggleMark"
    let params_to_json id =
      match id with
      | None -> None
      | Some id -> Some (`List ([`String id]))
    let result_of_json = result_to_state
  end

  module Sync_pane: Api_def with type params = unit
                          and type result = S.t
  = struct
    type result = S.t
    type params = unit

    let name = "pane/toggleMark"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  module Up_directory: Api_def with type params = unit
                                and type result = S.t
  = struct
    type result = S.t
    type params = unit

    let name = "pane/upDirectory"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  module Reload: Api_def with type params = unit
                                and type result = S.t
  = struct
    type result = S.t
    type params = unit

    let name = "pane/reload"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  module Enter_directory: Api_def with type params = unit
                                   and type result = S.t
  = struct
    type result = S.t
    type params = unit

    let name = "pane/enterDirectory"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end
end
