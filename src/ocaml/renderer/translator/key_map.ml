open Sxfiler_core
open Sxfiler_domain.Key_map
module T = Sxfiler_domain

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

let to_js t : js Js.t =
  let bindings = bindings t in
  let bindings = List.map (fun (cond, key, v) -> {Original_key_binding.condition = cond;
                                   key = Sxfiler_kbd.to_keyseq key;
                                   action = v;
                                  })
    bindings

  |> List.map Original_key_binding.to_js
  |> Array.of_list
  |> Js.array
  in
  object%js
    val bindings = bindings
  end

let of_js  (js:js Js.t) : string t =
  Js.array_map Original_key_binding.of_js js##.bindings
  |> Js.to_array
  |> Array.to_list
  |> List.fold_left (fun keymap v ->
      let open Original_key_binding in
      match Sxfiler_kbd.of_keyseq v.key with
      | None -> keymap
      | Some key -> add keymap ~condition:v.condition ~key ~value:v.action
    ) (make ())
