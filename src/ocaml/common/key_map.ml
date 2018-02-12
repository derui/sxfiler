module K = Sxfiler_kbd

type action = Key_bindable_action.t

module Key_map = Map.Make(struct
    type t = K.t
    let compare = Pervasives.compare
  end)

type t = action Key_map.t

let empty = Key_map.empty

let add_key_map ~key_map ~key ~action = Key_map.add key action key_map
let remove_key_map ~key_map ~key = Key_map.remove key key_map

(** Dispatch key to handler. Return a message if the handler binded with a key *)
let dispatch ~key_map ~key =
  let open Sxfiler_util.Option.Infix in
  Key_map.find_opt key key_map
