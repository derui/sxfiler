open Sxfiler_core
open Sxfiler_types

module R = Sxfiler_server.Rpc_connection

let rpc_connection = [
  Alcotest_lwt.test_case "can connect and disconnect" `Quick (fun _ () ->
      let conn = R.make () in
      let count = ref 0 in
      let writer _ = incr count in

      let%lwt _ = R.connect conn writer in
      let%lwt _ = R.with_conn (fun conn ->
          Lwt.return @@ R.write_output conn ~frame:(Some (Websocket_cohttp_lwt.Frame.create ()))
        ) in
      let%lwt _ = R.disconnect () in
      let%lwt _ = R.with_conn (fun conn ->
          Lwt.return @@ R.write_output conn ~frame:(Some (Websocket_cohttp_lwt.Frame.create ()))
        ) in
      Alcotest.(check int) "outputs" 1 !count;
      Lwt.return_unit
    );
  Alcotest_lwt.test_case "do nothing disconnect when no connection" `Quick (fun _ () ->
      let count = ref 0 in

      let%lwt () = R.disconnect () in
      let%lwt _ = R.with_conn (fun conn -> Lwt.return @@ incr count) in
      Alcotest.(check int) "not call" 0 !count;
      Lwt.return_unit
    );
  Alcotest_lwt.test_case "can not use disconnected connection twice" `Quick (fun _ () ->
      let conn = R.make () in
      let count = ref 0 in
      let writer _ = incr count in
      let twice_writer _ = count := !count + 2 in

      let%lwt _ = R.connect conn writer in
      let%lwt _ = R.with_conn (fun conn ->
          Lwt.return @@ R.write_output conn ~frame:(Some (Websocket_cohttp_lwt.Frame.create ()))
        ) in
      let%lwt _ = R.disconnect () in
      let%lwt _ = R.connect conn twice_writer in
      let%lwt _ = R.with_conn (fun conn ->
          Lwt.return @@ R.write_output conn ~frame:(Some (Websocket_cohttp_lwt.Frame.create ()))
        ) in
      let%lwt _ = R.disconnect () in
      Alcotest.(check int) "outputs" 1 !count;
      Lwt.return_unit
    );

  Alcotest_lwt.test_case "kill previous connection" `Quick (fun _ () ->
      let conn = R.make () in
      let conn' = R.make () in
      let count = ref 0 in
      let writer _ = incr count in
      let twice_writer _ = count := !count + 2 in

      let%lwt _ = R.connect conn writer in
      let%lwt _ = R.with_conn (fun conn ->
          Lwt.return @@ R.write_output conn ~frame:(Some (Websocket_cohttp_lwt.Frame.create ()))
        ) in
      let%lwt _ = R.connect conn' twice_writer in
      let%lwt _ = R.with_conn (fun conn ->
          Lwt.return @@ R.write_output conn ~frame:(Some (Websocket_cohttp_lwt.Frame.create ()))
        ) in
      let%lwt _ = R.disconnect () in
      (* can not connect with killed connection *)
      let%lwt _ = R.connect conn writer in
      let%lwt _ = R.with_conn (fun conn ->
          Lwt.return @@ R.write_output conn ~frame:(Some (Websocket_cohttp_lwt.Frame.create ()))
        ) in
      let%lwt _ = R.disconnect () in
      Alcotest.(check int) "outputs" 3 !count;
      Lwt.return_unit
    );
  Alcotest_lwt.test_case "can handle ping frame as default behavior" `Quick (fun _ () ->
      let conn = R.make () in
      let writer f =
        let module W = Websocket_cohttp_lwt in
        let expected = Some (W.Frame.create ~opcode:W.Frame.Opcode.Pong ~content:"foo" ()) in
        Alcotest.(check @@ of_pp @@ Fmt.nop) "frame" expected f
      in
      let%lwt _ = R.connect conn writer in
      let%lwt _ = R.with_conn (fun conn ->
          let module W = Websocket_cohttp_lwt in
          R.default_input_handler conn W.(Frame.create ~opcode:Frame.Opcode.Ping ~content:"foo" ());
        ) in
      let%lwt _ = R.disconnect () in
      Lwt.return_unit
    );
  "raise Exit if Close frame given", `Quick, (fun () ->
      Alcotest.check_raises "exit" Exit (fun () ->
          let conn = R.make () in

          let thread = let%lwt _ = R.connect conn ignore in
            let%lwt _ = R.with_conn (fun conn ->
                let module W = Websocket_cohttp_lwt in
                R.default_input_handler conn W.(Frame.create ~opcode:Frame.Opcode.Close ())
              )
            in
            let%lwt _ = R.disconnect () in
            Lwt.return_unit
          in
          Lwt_main.run thread |> ignore
        )
    );
]

let testcases = [
  "rpc_connection", rpc_connection;
]
