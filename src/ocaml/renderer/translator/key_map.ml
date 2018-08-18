open Sxfiler_core
module T = Sxfiler_rpc.Types

module Original_key_binding = struct
  type t = {
    key: string;
    action: string;
    condition: T.Condition.t;
  }

  class type js = object
    method key: Js.js_string Js.t Js.readonly_prop
    method action: Js.js_string Js.t Js.readonly_prop
    method _when: Condition.js Js.t Js.optdef Js.readonly_prop
  end

  let to_js t : js Js.t = object%js
    val key = Js.string t.key
    val action = Js.string t.action
    val _when = Js.Optdef.return @@ Condition.to_js t.condition
  end

  let of_js (js:js Js.t) : t = {
    key = Js.to_string js##.key;
    action = Js.to_string js##.action;
    condition = Js.Optdef.map js##._when Condition.of_js
                |> Js.Optdef.to_option
                |> Option.get ~default:(fun () -> T.Condition.empty);
  }
end

class type js = object
  method bindings: Original_key_binding.js Js.t Js.js_array Js.t Js.readonly_prop
end

let to_js (t:T.Key_map.t) : js Js.t =
  let bindings = t.T.Key_map.bindings in
  let bindings = List.map (fun v -> {
        Original_key_binding.condition = v.T.Key_map.condition;
        key = v.T.Key_map.key;
        action = v.action;
      })
    bindings

  |> List.map Original_key_binding.to_js
  |> Array.of_list
  |> Js.array
  in
  object%js
    val bindings = bindings
  end

let of_js (js:js Js.t) : T.Key_map.t =
  let bindings = Js.array_map Original_key_binding.of_js js##.bindings
                 |> Js.to_array
                 |> Array.to_list
                 |> List.map (fun v -> {
                       T.Key_map.key = v.Original_key_binding.key;
                       condition = v.Original_key_binding.condition;
                       action = v.action
                     }) in
  {T.Key_map.bindings = bindings}
