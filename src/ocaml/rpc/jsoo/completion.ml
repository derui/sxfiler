module T = Sxfiler_types
module Tj = Sxfiler_types_jsoo
module Rpc = Sxfiler_rpc

module Setup_sync = struct
  open Rpc.Completion.Setup_sync

  class type js_param = object
    method source: Tj.Completion.Item.js Js.t Js.js_array Js.t Js.readonly_prop
  end

  let params_to_json : params -> js_param Js.t = fun t -> object%js
    val source = List.map Tj.Completion.Item.to_js t.source
                 |> Array.of_list
                 |> Js.array
  end

  let result_of_json _ = ()
end

module Read_sync = struct
  open Rpc.Completion.Read_sync

  module Jsoo = struct
    class type params = object
      method input: Js.js_string Js.t Js.readonly_prop
    end

    type result = Tj.Completion.Candidate.js Js.t Js.js_array
  end

  let params_to_json t = object%js
    val input = Js.string t.input
  end

  let result_of_json : Jsoo.result Js.t -> result = fun js ->
    Js.to_array @@ Js.array_map (fun v -> Tj.Completion.Candidate.of_js v) js
end
