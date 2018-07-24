open Sxfiler_core
module R = Sxfiler_rpc
module Rj = Sxfiler_rpc_jsoo
module type Api_def = Rpc_intf.Api_def

(** Completion APIs *)
module Completion = struct

  module Setup_sync : Api_def with type params = R.Completion.Setup_sync.params
                               and type result = R.Completion.Setup_sync.result
  = struct
    include R.Completion.Setup_sync
    type json = < > Js.t

    open Rj.Completion.Setup_sync

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v -> Js.Unsafe.coerce @@ params_to_json v
    let result_of_json = result_of_json
  end

  module Read_sync : Api_def with type params = R.Completion.Read_sync.params
                              and type result = R.Completion.Read_sync.result
  = struct
    include R.Completion.Read_sync
    type json = < > Js.t

    open Rj.Completion.Read_sync

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v -> Js.Unsafe.coerce @@ params_to_json v
    let result_of_json v = result_of_json @@ Js.Unsafe.coerce v
  end

end

module Scanner = struct
  module Make_sync : Api_def with type params = R.Scanner.Make_sync.params
                              and type result = R.Scanner.Make_sync.result
  = struct
    include R.Scanner.Make_sync
    type json = < > Js.t

    open Rj.Scanner.Make_sync

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v -> Js.Unsafe.coerce @@ params_to_json v
    let result_of_json _ = ()

  end

  module Get_sync : Api_def with type params = R.Scanner.Get_sync.params
                             and type result = R.Scanner.Get_sync.result
  = struct
    include R.Scanner.Get_sync
    type json = < > Js.t

    open Rj.Scanner.Get_sync

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v -> Js.Unsafe.coerce @@ params_to_json v
    let result_of_json v = result_of_json @@ Js.Unsafe.coerce v

  end

end

module Keybindings = struct
  module Get_sync : Api_def with type params = Rj.Keybindings.Get_sync.params
                             and type result = Rj.Keybindings.Get_sync.result = struct
    include Rj.Keybindings.Get_sync
    type json = < > Js.t

    let params_to_json _ = None

    let result_of_json v = v
  end
end

module Configuration = struct
  module Get_sync : Api_def with type params = R.Configuration.Get_sync.params
                             and type result = R.Configuration.Get_sync.result = struct
    include R.Configuration.Get_sync
    type json = < > Js.t

    open Rj.Configuration.Get_sync

    let params_to_json _ = None
    let result_of_json v = result_of_json @@ Js.Unsafe.coerce v
  end
end
