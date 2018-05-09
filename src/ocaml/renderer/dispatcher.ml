module C_= Sxfiler_common
module K = Sxfiler_common.Key_map
module E = Jsoo_reactjs.Event

type t = Action_creator.action -> unit

let make store rpc =
  fun action ->
    (* TODO: convert action to api with publisher handler. *)
    let open Lwt.Infix in
    let lwt = action {Context.state = store.Store.state;rpc} >>=
      fun state -> Lwt.return @@ Store.update store state
    in
    Lwt.ignore_result lwt

let dispatch ~dispatcher action = dispatcher action
