open Sxfiler_rpc.Types.Condition

class type js = object
  method enabledContexts: Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
end

let to_js t : js Js.t =
  object%js
    val enabledContexts = Js.array @@ Array.of_list @@ List.map Js.string t.enabled_contexts
  end

let of_js : js Js.t -> t = fun js ->
  {
    enabled_contexts = js##.enabledContexts |> Js.to_array |> Array.to_list
                       |> List.map Js.to_string;
  }
