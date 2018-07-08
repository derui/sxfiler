open Sxfiler_core
module R = Sxfiler_rpc
module Rj = Sxfiler_rpc_jsoo
module type Api_def = Jsonrpc_ocaml_jsoo.Client.Api_def

(** Completion APIs *)
module Completion = struct

  module Setup_file_sync : Api_def with type params = R.Completion.Setup_file_sync.params
                                    and type result = R.Completion.Setup_file_sync.result
  = struct
    include R.Completion.Setup_file_sync
    type json = < > Js.t

    open Rj.Completion.Setup_file_sync

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v -> Js.Unsafe.coerce @@ params_to_json v
    let result_of_json = result_of_json
  end

  module Read_file_sync : Api_def with type params = R.Completion.Read_file_sync.params
                                   and type result = R.Completion.Read_file_sync.result
  = struct
    include R.Completion.Read_file_sync
    type json = < > Js.t

    open Rj.Completion.Read_file_sync

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v -> Js.Unsafe.coerce @@ params_to_json v
    let result_of_json v = result_of_json @@ Js.Unsafe.coerce v
  end

end

module Workspace = struct
  module Make_sync : Api_def with type params = R.Workspace.Make_sync.params
                              and type result = R.Workspace.Make_sync.result
  = struct
    include R.Workspace.Make_sync
    type json = < > Js.t

    open Rj.Workspace.Make_sync

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v -> Js.Unsafe.coerce @@ params_to_json v
    let result_of_json v = result_of_json @@ Js.Unsafe.coerce v

  end

  module Get_sync : Api_def with type params = R.Workspace.Get_sync.params
                             and type result = R.Workspace.Get_sync.result
  = struct
    include R.Workspace.Get_sync
    type json = < > Js.t

    open Rj.Workspace.Get_sync

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v -> Js.Unsafe.coerce @@ params_to_json v
    let result_of_json v = result_of_json @@ Js.Unsafe.coerce v

  end

end
