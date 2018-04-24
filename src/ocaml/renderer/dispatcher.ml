module C_= Sxfiler_common
module K = Sxfiler_common.Key_map
module E = Jsoo_reactjs.Event

type t = Sxfiler_common.Message.t -> unit

let make store rpc =
  fun action ->
    (* TODO: convert action to api with publisher handler. *)
    ()

let dispatch ~dispatcher action = dispatcher action
