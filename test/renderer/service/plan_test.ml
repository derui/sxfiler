open Mocha_of_ocaml
open Mocha_of_ocaml_async
module E = Sxfiler_rpc.Errors
module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service
module Tr = Sxfiler_renderer_translator

let () =
  "Plan service implementation"
  >::: [ ( "should return configuration"
           >:- fun () ->
             let module D = Sxfiler_domain in
             let module Client = Util.Make_client (struct
                 let gen id =
                   let module R = Jsonrpc_ocaml_jsoo in
                   R.Response.{empty with id; result = Some (object%js end)}
               end) in
             let module S = S.Plan.Make (Client) in
             let%lwt actual = S.reject {workbench_id = "id"} in
             Lwt.return @@ assert_ok (() = actual) ) ]
