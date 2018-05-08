module K = Sxfiler_kbd

type action = Common_key_bindable_action.t

module Key_map = Map.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

type t = action Key_map.t

type js = Js.js_string Js.t Jstable.t

let empty = Key_map.empty

let add_key_map ~key_map ~key ~action = Key_map.add key action key_map
let remove_key_map ~key_map ~key = Key_map.remove key key_map

(** Dispatch key to handler. Return a message if the handler binded with a key *)
let dispatch ~key_map ~key =
  let open Minimal_monadic_caml.Option.Infix in
  Key_map.find_opt key key_map

(** Convert JS object to instance of key map type *)
let of_js : js -> t = fun js ->
  let keys = Jstable.keys js in
  let map = empty in
  List.fold_left (fun map key ->
      let value = Jstable.find js key |> Js.Optdef.to_option in
      match value with
      | None -> map
      | Some action -> begin
          let module A = Common_key_bindable_action in
          match K.of_keyseq @@ Js.to_string action with
          | None -> map
          | Some key -> begin
              add_key_map ~key_map:map ~key:(K.to_keyseq key) ~action:(A.of_string @@ Js.to_string action)
            end
        end
    ) map keys
