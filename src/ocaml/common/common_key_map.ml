module K = Sxfiler_kbd

type action = Callable_action.t

module Key_map = Map.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

type t = action Key_map.t

type js = < >

let empty = Key_map.empty

let add_key_map ~key_map ~key ~action = Key_map.add key action key_map
let remove_key_map ~key_map ~key = Key_map.remove key key_map

(** Dispatch key to handler. Return a message if the handler binded with a key *)
let find ~key_map ~key =
  let open Minimal_monadic_caml.Option.Infix in
  Key_map.find_opt key key_map

(** Convert JS object to instance of key map type *)
let of_js : js Js.t -> t = fun js ->
  let keys = Js.object_keys js |> Js.to_array |> Array.to_list in
  let map = empty in
  List.fold_left (fun map key ->
      let value = Js.Unsafe.get js key |> Js.Optdef.to_option in
      match (value, K.of_keyseq @@ Js.to_string key) with
      | (None, _) | (_, None) -> map
      | Some action, Some key -> begin
          let module A = Callable_action in
          add_key_map ~key_map:map ~key:(K.to_keyseq key) ~action:(A.of_string @@ Js.to_string action)
        end
    ) map keys
