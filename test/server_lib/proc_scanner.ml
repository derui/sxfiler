open Sxfiler_core

module D = Sxfiler_domain
module S = Sxfiler_server
module U = Sxfiler_usecase
module I = Sxfiler_server_infra
module C = Sxfiler_server_core
module R = Sxfiler_rpc
module Jy = Jsonrpc_ocaml_yojson
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator

module Dummy_system = struct
  let getcwd () = "/foo"
end

let proc_scanner = [
  Alcotest_lwt.test_case "create new scanner if it does not exists" `Quick (fun switch () ->
      let expected = D.Scanner.make
          ~id:"foo"
          ~location:(Path.of_string ~env:`Unix "/initial")
          ~nodes:[]
          ~history:D.Location_history.(make ()) in

      let module Gateway = struct
        type params = {
          initial_location: string;
          name: string;
        } [@@deriving yojson]

        type result = {
          scanner: R.Types.Scanner.t option;
          already_exists: bool;
        }

        let handle _ = Lwt.return
            {scanner = Option.some @@ T.Scanner.of_domain expected;
             already_exists = false;
            }
      end in
      let module Make = S.Proc_scanner.Make(Gateway) in

      let%lwt res = Make.handle {
          Gateway.initial_location = "/initial";
          name = "foo"
        } in

      Alcotest.(check @@ of_pp @@ Fmt.nop) "created" (T.Scanner.of_domain expected) res;
      Lwt.return_unit
    );

  Alcotest_lwt.test_case "do not create workspace if it exists" `Quick (fun switch () ->
      let module Gateway = struct
        type params = {
          initial_location: string;
          name: string;
        } [@@deriving yojson]

        type result = {
          scanner: R.Types.Scanner.t option;
          already_exists: bool;
        }

        let handle _ = Lwt.return
            {scanner = None;
             already_exists = true;
            }
      end in
      let module Make = S.Proc_scanner.Make(Gateway) in

      let expected = Jy.(Exception.Jsonrpc_error (R.Errors.Scanner.already_exists, None)) in
      Alcotest.check_raises "raised" expected (fun () ->
          Lwt.ignore_result @@ Make.handle {
              initial_location = "/initial";
              name = "foo"
            }
        );
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : scanner", proc_scanner;
]
