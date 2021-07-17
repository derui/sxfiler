open Sxfiler_core
module A = Sxfiler_infrastructure.Ws_actor
module C = Sxfiler_infrastructure.Ws_connection
module W = Websocket

let test_set =
  let get_conn ?write_output () =
    (module struct
      let stream, push = Lwt_stream.create ()

      module Connection = struct
        type t = unit

        let write_output () ~frame = match write_output with None -> () | Some f -> f frame

        let process_input () ~f = Lwt_stream.iter_s f stream

        let push_input () ~frame = push frame

        let connect () _ = Lwt.return_unit

        let disconnect () =
          push None;
          Lwt_stream.closed stream |> Lwt.map (Fun.const ())

        let is_closed () = Lwt_stream.is_closed stream

        let default_input_handler () _ = Lwt.return_unit
      end

      let instance = ()
    end : C.Instance)
  in
  let module Gen = struct
    type id = Uuidm.t

    let state = Random.get_state ()

    let generate () = Uuidm.v4_gen state ()
  end in
  [
    Alcotest_lwt.test_case "receive frames with receiver" `Quick (fun _ () ->
        let module C = (val get_conn ()) in
        let module A' = (val A.make (module Gen) (module C)) in
        let actor = A'.(Actor.start instance) in
        let expected = W.Frame.create ~opcode:W.Frame.Opcode.Text ~content:"content" () in
        let waiter, wakener = Lwt.wait () in
        let receiver frame =
          Alcotest.(check & of_pp W.Frame.pp) "received" expected frame;
          Lwt.wakeup wakener ();
          Lwt.return `Continue
        in
        let%lwt () = A'.(Actor.add_receiver instance receiver) in
        C.(Connection.push_input instance ~frame:(Some expected));
        A'.(Actor.stop instance);
        let%lwt () = Lwt.join [ waiter; actor ] in
        Lwt.return_unit);
    Alcotest_lwt.test_case "can not send if sender was canceled" `Quick (fun _ () ->
        let expected = W.Frame.create ~opcode:W.Frame.Opcode.Text ~content:"content" () in
        let waiter, wakener = Lwt.wait () in
        let callee frame =
          Alcotest.(check & option & of_pp W.Frame.pp) "sent frame" (Some expected) frame;
          Lwt.wakeup wakener ()
        in
        let module C = (val get_conn ~write_output:callee ()) in
        let module A' = (val A.make (module Gen) (module C)) in
        let actor = A'.(Actor.start instance) in
        let sender, _ = A'.(Actor.make_sender instance) in
        sender expected;
        A'.(Actor.stop instance);
        let%lwt () = Lwt.join [ actor; waiter ] in
        Lwt.return_unit);
    Alcotest_lwt.test_case "call all receivers each frame" `Quick (fun _ () ->
        let module C = (val get_conn ()) in
        let module A' = (val A.make (module Gen) (module C)) in
        let actor = A'.(Actor.start instance) in
        let frame_1 = W.Frame.create ~opcode:W.Frame.Opcode.Text ~content:"content" () in
        let frame_2 = W.Frame.create ~opcode:W.Frame.Opcode.Text ~content:"content2" () in
        let waiter_1, wakener_1 = Lwt.wait () and waiter_2, wakener_2 = Lwt.wait () in
        let count = ref 0 in
        let receiver_1 frame =
          if !count = 0 then (
            incr count;
            Lwt.return `Continue)
          else (
            Alcotest.(check & of_pp W.Frame.pp) "received" frame_2 frame;
            Lwt.wakeup wakener_1 ();
            Lwt.return `Continue)
        in
        let receiver_2 frame =
          (* do not call twice *)
          Alcotest.(check & of_pp W.Frame.pp) "received" frame_1 frame;
          Lwt.wakeup wakener_2 ();
          Lwt.return `Finished
        in
        let%lwt () = A'.(Actor.add_receiver instance receiver_1) in
        let%lwt () = A'.(Actor.add_receiver instance receiver_2) in
        C.(Connection.push_input instance ~frame:(Some frame_1));
        C.(Connection.push_input instance ~frame:(Some frame_2));
        A'.(Actor.stop instance);
        let%lwt () = Lwt.join [ waiter_1; waiter_2; actor ] in
        Lwt.return_unit);
  ]
