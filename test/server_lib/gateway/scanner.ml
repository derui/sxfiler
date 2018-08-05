open Sxfiler_core

module D = Sxfiler_domain
module S = Sxfiler_server
module U = Sxfiler_usecase
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator

module Dummy_system = struct
  let getcwd () = "/foo"
end

let scanner_tests = [
  Alcotest_lwt.test_case "create new scanner if it does not exists" `Quick (fun switch () ->
      let expected = D.Scanner.make
          ~id:"foo"
          ~location:(Path.of_string ~env:`Unix (module Dummy_system:System.S) "/initial")
          ~nodes:[]
          ~history:D.Location_history.(make ()) in

      let module Usecase = struct
        include U.Scanner.Make_type

        let execute _ = Lwt.return_ok expected
      end in
      let module Gateway = G.Scanner.Make(Dummy_system)(Usecase) in

      let%lwt res = Gateway.handle {
          Gateway.initial_location = "/initial";
          name = "foo"
        } in
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "created"
        (Option.some @@ T.Scanner.of_domain expected) res.scanner;
      Lwt.return_unit
    );

  Alcotest_lwt.test_case "do not create workspace if it exists" `Quick (fun switch () ->
      let module Usecase = struct
        include U.Scanner.Make_type

        let execute _ = Lwt.return_error (U.Common.MakeScannerError `Already_exists)
      end in
      let module Gateway = G.Scanner.Make(Dummy_system)(Usecase) in

      let%lwt res = Gateway.handle {
          initial_location = "/initial";
          name = "foo"
        } in
      Alcotest.(check bool) "error" true res.already_exists;
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : scanner", scanner_tests;
]
