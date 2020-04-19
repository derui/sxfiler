module I = Sxfiler_infrastructure
module W = Websocket

let make () =
  let stream, push = Lwt_stream.create () in
  let module A = struct
    module Actor = struct
      type t = { mutable receivers : I.Ws_actor.receiver list }

      let global_waiter, global_wakener = Lwt.wait ()

      let add_receiver t receiver =
        t.receivers <- receiver :: t.receivers;
        Lwt.return_unit

      let make_sender _ = ((fun _ -> ()), fun () -> ())

      let start t =
        Lwt.async (fun () ->
            Lwt_stream.iter_s
              (fun v -> Lwt_list.map_p (fun r -> r v) t.receivers |> Lwt.ignore_result |> Lwt.return)
              stream);
        Lwt.join [ global_waiter; Lwt_stream.closed stream ]

      let stop _ =
        Lwt.wakeup global_wakener ();
        push None
    end

    let instance = { Actor.receivers = [] }
  end in
  ((fun v -> push (Some v)), (module A : I.Ws_actor.Instance))
