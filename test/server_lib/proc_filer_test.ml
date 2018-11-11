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

let test_set =
  [ Alcotest_lwt.test_case "create new filer if it does not exists" `Quick (fun _ () ->
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
        let proc = S.Proc_filer.make_spec (module Gateway) in
        let id = Random.int64 Int64.max_int in
        let%lwt res =
          proc.S.Procedure_intf.handler
            Jy.Request.
              { id = Some id
              ; _method = ""
              ; params =
                  Gateway.params_to_yojson Gateway.{initial_location = "/initial"; name = "foo"}
                  |> Option.some }
        in
        Alcotest.(check @@ of_pp @@ Fmt.nop)
          "created" res
          Jy.Response.
            { id = Some id
            ; error = None
            ; result = Some (T.Filer.of_domain expected |> T.Filer.to_yojson) } ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "do not create workspace if it exists" `Quick (fun _ () ->
        let module Gateway = struct
          include G.Filer.Make.Types

          let handle _ = Lwt.return {filer = None; already_exists = true}
        end in
        let proc = S.Proc_filer.make_spec (module Gateway) in
        let expected = Jy.(Exception.Jsonrpc_error (R.Errors.Filer.already_exists, None)) in
        let id = Random.int64 Int64.max_int in
        Alcotest.check_raises "raised" expected (fun () ->
            proc.S.Procedure_intf.handler
              Jy.Request.
                { id = Some id
                ; _method = ""
                ; params =
                    Gateway.params_to_yojson Gateway.{initial_location = "/initial"; name = "foo"}
                    |> Option.some }
            |> Lwt.ignore_result ) ;
        Lwt.return_unit ) ]
