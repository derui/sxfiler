type action = Callable_action.t
type key = string

module Key_map = Map.Make(struct
    type t = key
    let compare = Pervasives.compare
  end)

type t = action Key_map.t

let empty = Key_map.empty

let add key_bindings ~key ~action = Key_map.add key action key_bindings
let remove key_bindings ~key = Key_map.remove key key_bindings

(** Dispatch key to handler. Return a message if the handler binded with a key *)
let find key_bindings ~key = Key_map.find_opt key key_bindings

let dump t = Key_map.fold (fun key v list -> (key, v) :: list) t []
