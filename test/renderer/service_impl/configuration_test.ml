open Mocha_of_ocaml
open Mocha_of_ocaml_async
module E = Sxfiler_rpc.Errors
module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module SI = Sxfiler_renderer_service_impl
module Tr = Sxfiler_renderer_translator

let () =
  "Configuration service implementation"
  >::: [ ( "should return configuration"
           >:- fun () ->
             let module D = Sxfiler_domain in
             let expected = T.Configuration.{default_sort_order = D.Types.Sort_type.(to_int Date)} in
             let module Client = Util.Make_client (struct
                 let gen id =
                   let module R = Jsonrpc_ocaml_jsoo in
                   R.Response.
                     {empty with id; result = Some (Js.Unsafe.coerce @@ Tr.Configuration.to_js expected)}
               end) in
             let module S = SI.Configuration.Make (Client) in
             let%lwt actual = S.get () in
             Lwt.return @@ assert_ok (expected = actual) ) ]
