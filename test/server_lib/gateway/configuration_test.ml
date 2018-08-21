open Sxfiler_core

module D = Sxfiler_domain
module S = Sxfiler_server
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module U = Sxfiler_usecase

let configuration_tests = [
  Alcotest_lwt.test_case "get current configuration" `Quick (fun switch () ->
      let expected = D.Configuration.{
          sort_order = D.Types.Sort_type.Date
        } in
      let module Usecase = struct
        type input = unit
        type output = D.Configuration.t

        let execute () = Lwt.return @@ Ok expected
        end in
      let module Gateway = G.Configuration.Get(Usecase) in

      let%lwt res = Gateway.handle () in
      Alcotest.(check @@ of_pp Fmt.nop) "current" (T.Configuration.of_domain expected) res;
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : configuration", configuration_tests;
]