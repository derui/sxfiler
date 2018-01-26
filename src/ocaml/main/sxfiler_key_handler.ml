module C = Sxfiler_common
module K = Sxfiler_kbd

type handler = unit -> C.Message.t

module Handler_map = Map.Make(struct
    type t = K.t
    let compare = Pervasives.compare
  end)

type handler_map = handler Handler_map.t

let empty : handler Handler_map.t = Handler_map.empty

let add_handler ~handlers ~key ~handler = Handler_map.add key handler handlers
let remove_handler ~handlers ~key = Handler_map.remove key handlers

(** Dispatch key to handler. Return a message if the handler binded with a key *)
let dispatch ~handlers ~key =
  let open C.Util.Option.Infix in
  Handler_map.find_opt key handlers >>= (fun handler -> return @@ handler ())
