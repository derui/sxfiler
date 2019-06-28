module R = Sxfiler_server_core.Rpc_connection

let test_set =
  [ Alcotest_lwt.test_case "can connect and disconnect" `Quick (fun _ () ->
        let conn = R.make () in
        let module C = (val conn : R.Instance) in
        let conn = C.instance in
        let count = ref 0 in
        let writer _ = incr count in
        let%lwt _ = C.Connection.connect conn writer in
        let%lwt _ =
          Lwt.return @@ C.Connection.write_output conn ~frame:(Some (Websocket.Frame.create ()))
        in
        let%lwt _ = C.Connection.disconnect conn in
        let%lwt _ =
          Lwt.return @@ C.Connection.write_output conn ~frame:(Some (Websocket.Frame.create ()))
        in
        Alcotest.(check int) "outputs" 1 !count ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "do nothing disconnect when not connected yet" `Quick (fun _ () ->
        let conn = R.make () in
        let module C = (val conn : R.Instance) in
        let conn = C.instance in
        let%lwt () = C.Connection.disconnect conn in
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "can not use disconnected connection twice" `Quick (fun _ () ->
        let module C = (val R.make () : R.Instance) in
        let module Conn = C.Connection in
        let conn = C.instance in
        let count = ref 0 in
        let writer _ = incr count in
        let twice_writer _ = count := !count + 2 in
        let%lwt _ = Conn.connect conn writer in
        let%lwt _ =
          Lwt.return @@ Conn.write_output conn ~frame:(Some (Websocket.Frame.create ()))
        in
        let%lwt _ = Conn.disconnect conn in
        let%lwt _ = Conn.connect conn twice_writer in
        let%lwt _ =
          Lwt.return @@ Conn.write_output conn ~frame:(Some (Websocket.Frame.create ()))
        in
        let%lwt _ = Conn.disconnect conn in
        Alcotest.(check int) "outputs" 1 !count ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "return pong opcode when ping given" `Quick (fun _ () ->
        let module C = (val R.make () : R.Instance) in
        let conn = C.instance in
        let module Conn = C.Connection in
        let frame = ref None in
        let module W = Websocket in
        let%lwt _ = Conn.connect conn (fun v -> frame := v) in
        let%lwt _ =
          Conn.default_input_handler conn W.(Frame.create ~opcode:Frame.Opcode.Ping ())
        in
        Alcotest.(check @@ option @@ of_pp Fmt.nop)
          "closed"
          (Some W.Frame.(create ~opcode:Opcode.Pong ()))
          !frame ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "disconnect when Close opcode given" `Quick (fun _ () ->
        let module C = (val R.make () : R.Instance) in
        let conn = C.instance in
        let module Conn = C.Connection in
        let%lwt _ = Conn.connect conn ignore in
        let%lwt _ =
          let module W = Websocket in
          Conn.default_input_handler conn W.(Frame.create ~opcode:Frame.Opcode.Close ())
        in
        Alcotest.(check bool) "closed" true Conn.(is_closed conn) ;
        Lwt.return_unit) ]
