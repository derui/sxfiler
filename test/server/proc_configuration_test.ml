module D = Sxfiler_domain
module S = Sxfiler_server
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types
module Jy = Jsonrpc_ocaml_yojson

let test_set =
  [ Alcotest_lwt.test_case "get current configuration" `Quick (fun _ () ->
        let expected = D.Configuration.{default_sort_order = D.Types.Sort_type.Date} in
        let module State = C.Statable.Make (struct
            type t = T.Configuration.t

            let empty () = Tr.Configuration.of_domain expected
          end) in
        let module Gateway = struct
          type params = unit
          type result = T.Configuration.t

          let handle () = State.get ()
        end in
        let spec = S.Proc_configuration.get_spec (module Gateway) in
        let id = Random.int64 Int64.max_int in
        let%lwt res =
          spec.S.Procedure.handler Jy.Request.{_method = ""; params = None; id = Some id}
        in
        Alcotest.(check @@ of_pp @@ Fmt.nop)
          "current" res
          Jy.Response.
            { id = Some id
            ; error = None
            ; result = Some (Tr.Configuration.of_domain expected |> Tr.Configuration.to_yojson) } ;
        Lwt.return_unit ) ]
