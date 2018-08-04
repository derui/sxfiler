open Sxfiler_core
open Sxfiler_domain.Key_map
module T = Sxfiler_domain

module type Conv = sig
  type t

  val to_json: t -> < > Js.t
  val of_json: < > Js.t -> t
end

module Original_key_binding = struct
  type 'a t = {
    key: string;
    value: 'a;
    condition: T.Condition.t;
  }

  class type js = object
    method key: Js.js_string Js.t Js.readonly_prop
    method value: < > Js.t Js.readonly_prop
    method _when: Condition.js Js.t Js.optdef Js.readonly_prop
  end

  let to_js (type v) (t:v t) ~conv:(module C:Conv with type t = v) : js Js.t = object%js
    val key = Js.string t.key
    val value = C.to_json t.value
    val _when = Js.Optdef.return @@ Condition.to_js t.condition
  end

  let of_js (type v) (js:js Js.t) ~conv:(module C:Conv with type t = v) : v t = {
    key = Js.to_string js##.key;
    value = C.of_json js##.value;
    condition = Js.Optdef.map js##._when Condition.of_js
                |> Js.Optdef.to_option
                |> Option.get ~default:(fun () -> T.Condition.empty);
  }
end

class type js = object
  method id: Js.js_string Js.t Js.readonly_prop
  method bindings: Original_key_binding.js Js.t Js.js_array Js.t Js.readonly_prop
end

let to_js (type v) (t:v t) ~conv:(module C:Conv with type t = v) : js Js.t =
  let bindings = bindings t in
  let bindings = List.map (fun (cond, key, v) -> {Original_key_binding.condition = cond;
                                   key = Sxfiler_kbd.to_keyseq key;
                                   value = v;
                                  })
    bindings

  |> List.map (Original_key_binding.to_js ~conv:(module C))
  |> Array.of_list
  |> Js.array
  in
  object%js
    val id = Js.string @@ id t
    val bindings = bindings
  end

let of_js (type v) (js:js Js.t) ~conv:(module C:Conv with type t = v) : v t =
  let id = Js.to_string js##.id in
  Js.array_map (Original_key_binding.of_js ~conv:(module C)) js##.bindings
  |> Js.to_array
  |> Array.to_list
  |> List.fold_left (fun keymap v ->
      let open Original_key_binding in
      match Sxfiler_kbd.of_keyseq v.key with
      | None -> keymap
      | Some key -> add keymap ~condition:v.condition ~key ~value:v.value
    ) (make id)
