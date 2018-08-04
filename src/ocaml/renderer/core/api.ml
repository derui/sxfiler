open Sxfiler_core
module P = Sxfiler_renderer_presenter
module R = Sxfiler_rpc
module type Api_def = Jsonrpc_ocaml_jsoo.Client.Api_def

(** Completion APIs *)
module Completion = struct

  module Setup_sync : Api_def with type params = R.Completion.Setup_sync.params
                               and type result = R.Completion.Setup_sync.result
  = struct
    include R.Completion.Setup_sync
    type json = < > Js.t

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v ->
      let params = object%js
        val source = List.map P.Completion.Item.to_js v.source
                     |> Array.of_list
                     |> Js.array
      end in
      Js.Unsafe.coerce params

    let result_of_json _ = ()
  end

  module Read_sync : Api_def with type params = R.Completion.Read_sync.params
                              and type result = R.Completion.Read_sync.result
  = struct
    include R.Completion.Read_sync
    type json = < > Js.t

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v ->
      let params = object%js
        val input = Js.string v.input
      end in
      Js.Unsafe.coerce params
    let result_of_json v =
      let v = Js.Unsafe.coerce v in
      Js.to_array @@ Js.array_map (fun v -> P.Completion.Candidate.of_js v) v
  end

end

module Scanner = struct
  module Make_sync : Api_def with type params = R.Scanner.Make_sync.params
                              and type result = R.Scanner.Make_sync.result
  = struct
    include R.Scanner.Make_sync
    type json = < > Js.t

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v ->
      Js.Unsafe.coerce (object%js
        val initialLocation = Js.string v.initial_location
        val name = Js.string v.name
      end)

    let result_of_json _ = ()

  end

  module Get_sync : Api_def with type params = R.Scanner.Get_sync.params
                             and type result = R.Scanner.Get_sync.result
  = struct
    include R.Scanner.Get_sync
    type json = < > Js.t

    let params_to_json params =
      let open Option.Infix in
      params >|= fun v -> Js.Unsafe.coerce (object%js
        val name = Js.string v.name
      end)

    let result_of_json v = P.Scanner.of_js @@ Js.Unsafe.coerce v
  end

end

module Keymap = struct
  module Get_sync : Api_def with type params = R.Keymap.Get_sync.params
                             and type result = R.Keymap.Get_sync.result = struct
    include R.Keymap.Get_sync
    type json = < > Js.t

    let params_to_json _ = None

    let result_of_json v =
      let v = Js.Unsafe.coerce v in
      P.Key_map.of_js ~conv:(module struct
                              type t = string
                              let to_json t = Js.Unsafe.coerce @@ Js.string t
                              let of_json js = Js.Unsafe.coerce js |> Js.to_string
                            end)
        v
  end
end

module Configuration = struct
  module Get_sync : Api_def with type params = R.Configuration.Get_sync.params
                             and type result = R.Configuration.Get_sync.result = struct
    include R.Configuration.Get_sync
    type json = < > Js.t

    let params_to_json _ = None
    let result_of_json v = P.Configuration.of_js @@ Js.Unsafe.coerce v
  end
end
