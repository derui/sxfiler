module K = Sxfiler_kbd
include Sxfiler_types.Key_map
type js = < >

(** Convert JS object to instance of key map type *)
let of_js : js Js.t -> t = fun js ->
  let keys = Js.object_keys js |> Js.to_array |> Array.to_list in
  let map = empty in
  List.fold_left (fun map key ->
      let value = Js.Unsafe.get js key |> Js.Optdef.to_option in
      match (value, K.of_keyseq @@ Js.to_string key) with
      | (None, _) | (_, None) -> map
      | Some action, Some key -> begin
          let module A = Sxfiler_types.Callable_action in
          add map ~key:(K.to_keyseq key) ~action:(A.of_string @@ Js.to_string action)
        end
    ) map keys
