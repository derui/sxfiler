open Sxfiler_core

module D = Sxfiler_domain
module S = Sxfiler_server
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

let proc_configuration = [
  Alcotest_lwt.test_case "get current configuration" `Quick (fun switch () ->
      let expected = D.Configuration.{sort_order = D.Types.Sort_type.Date} in
      let module State = C.Statable.Make(struct
          type t = T.Configuration.t
          let empty () = Tr.Configuration.of_domain expected
        end) in
      let module Gateway = struct
        type params = unit
        type result = T.Configuration.t

        let handle () = State.get ()
      end in
      let module Get = S.Proc_configuration.Get(Gateway) in

      let%lwt res = Get.handle () in
      let%lwt actual = State.get () in
      Alcotest.(check @@ of_pp @@ Fmt.nop) "current" expected @@ Tr.Configuration.to_domain actual;
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : confguration", proc_configuration;
]
