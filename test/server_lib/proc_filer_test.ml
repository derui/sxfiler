open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_server
module U = Sxfiler_usecase
module R = Sxfiler_rpc
module Jy = Jsonrpc_ocaml_yojson
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator

module Dummy_system = struct
  let getcwd () = "/foo"
end

let proc_filer =
  [ Alcotest_lwt.test_case "create new filer if it does not exists" `Quick (fun switch () ->
        let expected =
          D.Filer.make ~id:"foo"
            ~location:(Path.of_string ~env:`Unix "/initial")
            ~nodes:[] ~sort_order:D.Types.Sort_type.Date
            ~history:D.Location_history.(make ())
        in
        let module Gateway = struct
          include G.Filer.Make.Types

          let handle _ =
            Lwt.return {filer = Option.some @@ T.Filer.of_domain expected; already_exists = false}
        end in
        let module Make = S.Proc_filer.Make (Gateway) in
        let%lwt res = Make.handle {Gateway.initial_location = "/initial"; name = "foo"} in
        Alcotest.(check @@ of_pp @@ Fmt.nop) "created" (T.Filer.of_domain expected) res ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "do not create workspace if it exists" `Quick (fun switch () ->
        let module Gateway = struct
          include G.Filer.Make.Types

          let handle _ = Lwt.return {filer = None; already_exists = true}
        end in
        let module Make = S.Proc_filer.Make (Gateway) in
        let expected = Jy.(Exception.Jsonrpc_error (R.Errors.Filer.already_exists, None)) in
        Alcotest.check_raises "raised" expected (fun () ->
            Lwt.ignore_result @@ Make.handle {initial_location = "/initial"; name = "foo"} ) ;
        Lwt.return_unit ) ]

let () = Alcotest.run "Filer procedures" [("make", proc_filer)]
