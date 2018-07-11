module K = Sxfiler_types.Key_map
module E = Jsoo_reactjs.Event

type t = action -> unit

let make store rpc =
  fun action ->
    let open Lwt.Infix in
    let lwt = action {Context.state = store.Store.state;rpc} >>=
      fun state -> Lwt.return @@ Store.update store state
    in
    Lwt.ignore_result lwt
