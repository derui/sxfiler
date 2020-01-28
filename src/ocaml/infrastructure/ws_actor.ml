open Sxfiler_core
include Ws_actor_intf
module W = Websocket
module D = Sxfiler_domain

module L = (val Logger.make [ "ws_actor" ])

module Receiver_map = Map.Make (struct
  type t = Uuidm.t

  let compare = Uuidm.compare
end)

module Impl (IG : D.Id_generator.S with type id = Uuidm.t) = struct
  type t = {
    conn : (module Ws_connection.Instance);
    mutable receivers : receiver Receiver_map.t;
    receiver_lock : Lwt_mutex.t;
    waiter : unit Lwt.t;
    wakener : unit Lwt.u;
  }

  let add_receiver t receiver =
    Lwt_mutex.with_lock t.receiver_lock (fun () ->
        let id = IG.generate () in
        L.debug (fun m -> m "Add receiver to actor system");%lwt
        t.receivers <- Receiver_map.add id receiver t.receivers;
        Lwt.return_unit)

  let make_sender t =
    let module C = (val t.conn) in
    let conn = ref (Some t.conn) in
    let sender f =
      match !conn with None -> () | Some (module C) -> C.(Connection.write_output instance ~frame:(Some f))
    in
    let cancel () = conn := None in
    Lwt.ignore_result & L.debug (fun m -> m "Make a sender");
    (sender, cancel)

  let start t =
    let module C = (val t.conn) in
    let handle () =
      C.(
        Connection.process_input instance ~f:(fun frame ->
            L.debug (fun m -> m "Start handle a frame");%lwt
            let%lwt receivers = Lwt_mutex.with_lock t.receiver_lock (fun () -> Lwt.return t.receivers) in
            let receivers' = Receiver_map.to_seq receivers |> List.of_seq in
            let%lwt results =
              Lwt_list.map_p
                (fun (id, r) ->
                  let%lwt result = r frame in
                  Lwt.return (id, result))
                receivers'
            in
            let new_receivers =
              List.filter (fun (_, ret) -> match ret with `Finished -> true | `Continue -> false) results
              |> List.fold_left (fun map (id, _) -> Receiver_map.remove id map) receivers
            in
            Lwt_mutex.with_lock t.receiver_lock (fun () ->
                t.receivers <- new_receivers;
                Lwt.return_unit);%lwt
            L.debug (fun m -> m "Finish handling a frame")))
    in
    Lwt.async handle;
    t.waiter

  let stop t = Lwt.wakeup t.wakener ()
end

let make (module IG : D.Id_generator.S with type id = Uuidm.t) (module C : Ws_connection.Instance) =
  let waiter, wakener = Lwt.wait () in
  let module IM = Impl (IG) in
  ( module struct
    module Actor = IM

    let instance =
      { IM.conn = (module C); receivers = Receiver_map.empty; receiver_lock = Lwt_mutex.create (); waiter; wakener }
  end : Instance )
