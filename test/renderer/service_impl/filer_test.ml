open Mocha_of_ocaml
open Mocha_of_ocaml_async
module E = Sxfiler_rpc.Errors
module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module SI = Sxfiler_renderer_service_impl
module Tr = Sxfiler_renderer_translator

let () =
  "Filer service implementation"
  >::: [ ( "should return filer if it created"
           >:- fun () ->
             let expected =
               T.Filer.{id = "foo"; location = "loc"; nodes = []; history = T.Location_history.empty}
             in
             let module Client = Util.Make_client (struct
                 let gen id =
                   let module R = Jsonrpc_ocaml_jsoo in
                   R.Response.
                     {empty with id; result = Some (Js.Unsafe.coerce @@ Tr.Filer.to_js expected)}
               end) in
             let module S = SI.Filer.Make (Client) in
             match%lwt S.make {initial_location = "loc"; name = "bar"} with
             | Error _ -> Lwt.return @@ assert_fail "Unknown branch"
             | Ok actual -> Lwt.return @@ assert_ok (expected = actual) )
       ; ( "should return error if it already created"
           >:- fun () ->
             let module Client = Util.Make_client (struct
                 let gen id =
                   let module R = Jsonrpc_ocaml_jsoo in
                   R.Response.
                     {empty with id; error = Some R.Error.{code = E.Filer.already_exists; data = None}}
               end) in
             let module S = SI.Filer.Make (Client) in
             match%lwt S.make {initial_location = "loc"; name = "bar"} with
             | Error e -> Lwt.return @@ assert_ok (e = `Already_exists)
             | Ok _ -> Lwt.return @@ assert_fail "Unknown branch" )
       ; ( "should raise error when get response another error"
           >:- fun () ->
             let module Client = Util.Make_client (struct
                 let gen id =
                   let module R = Jsonrpc_ocaml_jsoo in
                   R.Response.
                     { empty with
                       id; error = Some R.Error.{code = R.Types.Error_code.Internal_error; data = None}
                     }
               end) in
             let module S = SI.Filer.Make (Client) in
             match%lwt S.make {initial_location = "loc"; name = "bar"} with
             | exception Sxfiler_core.Error.Error _ -> Lwt.return @@ assert_ok true
             | _ -> Lwt.return @@ assert_fail "Unknown branch" ) ] ;
  "Filer service implementation: Get"
  >::: [ ( "should return filer if it exists"
           >:- fun () ->
             let expected =
               T.Filer.{id = "foo"; location = "loc"; nodes = []; history = T.Location_history.empty}
             in
             let module Client = Util.Make_client (struct
                 let gen id =
                   let module R = Jsonrpc_ocaml_jsoo in
                   R.Response.
                     {empty with id; result = Some (Js.Unsafe.coerce @@ Tr.Filer.to_js expected)}
               end) in
             let module S = SI.Filer.Make (Client) in
             match%lwt S.get {name = "bar"} with
             | Error _ -> Lwt.return @@ assert_fail "Unknown branch"
             | Ok actual -> Lwt.return @@ assert_ok (expected = actual) )
       ; ( "should return error if it do not find"
           >:- fun () ->
             let module Client = Util.Make_client (struct
                 let gen id =
                   let module R = Jsonrpc_ocaml_jsoo in
                   R.Response.
                     {empty with id; error = Some R.Error.{code = E.Filer.not_found; data = None}}
               end) in
             let module S = SI.Filer.Make (Client) in
             match%lwt S.get {name = "bar"} with
             | Error e -> Lwt.return @@ assert_ok (e = `Not_found)
             | Ok _ -> Lwt.return @@ assert_fail "Unknown branch" )
       ; ( "should raise error when get response another error"
           >:- fun () ->
             let module Client = Util.Make_client (struct
                 let gen id =
                   let module R = Jsonrpc_ocaml_jsoo in
                   R.Response.
                     { empty with
                       id; error = Some R.Error.{code = R.Types.Error_code.Internal_error; data = None}
                     }
               end) in
             let module S = SI.Filer.Make (Client) in
             match%lwt S.get {name = "bar"} with
             | exception Sxfiler_core.Error.Error _ -> Lwt.return @@ assert_ok true
             | _ -> Lwt.return @@ assert_fail "Unknown branch" ) ]
