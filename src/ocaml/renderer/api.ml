module C = Sxfiler_common
module S = Sxfiler_common.Server_state
module type Api_def = Jsonrpc_ocaml_jsoo.Client.Api_def

(** Root level APIs *)
module Root = struct
  (* It is very unsafe to convert from js object having unknown type to State object, but
     unsave conversions takes only this place.
  *)
  let result_to_state js = S.of_js @@ Js.Unsafe.coerce js

  (** Get current state *)
  module Get_current_state : Api_def with type params = unit
                                      and type result = S.t
  = struct
    type result = S.t
    type params = unit
    type json = < > Js.t

    let name = "getCurrentState"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  (** Mapping pane/swapActivePane *)
  module Take_snapshot : Api_def with type params = unit
                                     and type result = unit
  = struct
    type result = unit
    type params = unit
    type json = < > Js.t

    let name = "takeSnapshot"
    let params_to_json _ = None
    let result_of_json _ = ()
  end
end

(** APIs for config  *)
module Config = struct
  let result_to_config result = C.Config.of_js @@ Js.Unsafe.coerce result

  module Current : Api_def with type params = unit
                            and type result = C.Config.t
  = struct
    type result = C.Config.t
    type params = unit
    type json = < > Js.t

    let name = "config/current"
    let params_to_json _ = None
    let result_of_json = result_to_config
  end

  module Load : Api_def with type params = unit
                            and type result = C.Config.t
  = struct
    type result = C.Config.t
    type params = unit
    type json = < > Js.t

    let name = "config/load"
    let params_to_json _ = None
    let result_of_json = result_to_config
  end
end

(** APIs for config  *)
module Pane = struct
  let result_to_state result = S.of_js @@ Js.Unsafe.coerce result

  module Move_focus : Api_def with type params = int
                               and type result = S.t
  = struct
    type result = S.t
    type params = int
    type json = < > Js.t

    let name = "pane/moveFocus"
    let params_to_json amount =
      match amount with
      | None -> None
      | Some amount -> Some (Js.Unsafe.coerce @@ Js.array [|amount|])
    let result_of_json = result_to_state
  end

  module Sync_pane: Api_def with type params = unit
                             and type result = S.t
  = struct
    type result = S.t
    type params = unit
    type json = < > Js.t

    let name = "pane/toggleMark"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  module Up_directory: Api_def with type params = unit
                                and type result = S.t
  = struct
    type result = S.t
    type params = unit
    type json = < > Js.t

    let name = "pane/upDirectory"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  module Reload: Api_def with type params = unit
                                and type result = S.t
  = struct
    type result = S.t
    type params = unit
    type json = < > Js.t

    let name = "pane/reload"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  module Enter_directory: Api_def with type params = unit
                                   and type result = S.t
  = struct
    type result = S.t
    type params = unit
    type json = < > Js.t

    let name = "pane/enterDirectory"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  module Swap_active: Api_def with type params = unit
                               and type result = S.t
  = struct
    type result = S.t
    type params = unit
    type json = < > Js.t

    let name = "pane/swapActive"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

  module Jump: Api_def with type params = string
                        and type result = S.t
  = struct
    type result = S.t
    type params = string
    type json = < > Js.t

    let name = "pane/jump"
    let params_to_json = function
      | None -> None
      | Some path -> begin
          Some (Js.Unsafe.coerce @@ Js.array [|Js.string path|])
        end
    let result_of_json = result_to_state
  end
end

(** APIs for file operation  *)
module File = struct
  let result_to_state result = S.of_js @@ Js.Unsafe.coerce result

  module Copy : Api_def with type params = unit
                         and type result = S.t
  = struct
    type result = S.t
    type params = unit
    type json = < > Js.t

    let name = "file/copy"
    let params_to_json _ = None
    let result_of_json = result_to_state
  end

end

(** APIs for completer operation  *)
module Completer = struct
  type state = C.Completer_state.t
  let result_to_state result = C.Completer_state.of_js @@ Js.Unsafe.coerce result

  (** Initialize completer *)
  module Initialize : Api_def with type params = [`File_list | `History]
                               and type result = state
  = struct
    type result = state
    type params = [`File_list | `History]
    type json = < > Js.t

    let name = "completer/initialize"
    let params_to_json = function
      | None -> None
      | Some typ -> begin
          match typ with
          | `File_list -> Some (Js.Unsafe.coerce @@ Js.array [|Js.string "file-list"|])
          | `History -> Some (Js.Unsafe.coerce @@ Js.array [|Js.string "history"|])
        end
    let result_of_json = result_to_state
  end

  (** Match candidates with given string *)
  module Match : Api_def with type params = string
                               and type result = state
  = struct
    type result = state
    type params = string
    type json = < > Js.t

    let name = "completer/match"
    let params_to_json = function
      | None -> None
      | Some text -> Some (Js.Unsafe.coerce @@ Js.array [|Js.string text|])
    let result_of_json = result_to_state
  end
end
