open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module T = Sxfiler_rpc.Types
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase

let () =
  "Initialize filer usecase"
  >::: [ ( "should return filer when make operation returns no error"
           >:- fun () ->
             let expected =
               { T.Filer.id = "foo"
               ; location = "loc"
               ; nodes = []
               ; history = T.Location_history.{records = []; max_records = 100} }
             in
             let module S : S.Filer.S = struct
               include Util.Service_stub.Filer

               let make _ = Lwt.return_ok expected
             end in
             let module Target = U.Initialize_filer.Make (S) in
             let instance = Target.create {initial_location = "foo"; pos = `Left} in
             let module D = struct
               let message_ = ref C.Message.Swap_filer
               let dispatch m = message_ := m
             end in
             let%lwt () = Target.execute instance (module Util.Dummy_dispatcher (D)) in
             Lwt.return @@ assert_ok (!D.message_ = C.Message.Update_filer (`Left, expected)) )
       ; ( "should get filer when make operation returns error"
           >:- fun () ->
             let expected =
               { T.Filer.id = "foo"
               ; location = "loc"
               ; nodes = []
               ; history = T.Location_history.{records = []; max_records = 100} }
             in
             let module S : S.Filer.S = struct
               include Util.Service_stub.Filer

               let make _ = Lwt.return_error `Already_exists
               let get _ = Lwt.return_ok expected
             end in
             let module Target = U.Initialize_filer.Make (S) in
             let instance = Target.create {initial_location = "foo"; pos = `Left} in
             let module D = struct
               let message_ = ref C.Message.Swap_filer
               let dispatch m = message_ := m
             end in
             let%lwt () = Target.execute instance (module Util.Dummy_dispatcher (D)) in
             Lwt.return @@ assert_ok (!D.message_ = C.Message.Update_filer (`Left, expected)) )
       ; ( "should not send message all operation failed"
           >:- fun () ->
             let module S : S.Filer.S = struct
               include Util.Service_stub.Filer

               let make _ = Lwt.return_error `Already_exists
               let get _ = Lwt.return_error `Not_found
             end in
             let module Target = U.Initialize_filer.Make (S) in
             let instance = Target.create {initial_location = "foo"; pos = `Left} in
             let module D = struct
               let count = ref 0
               let dispatch _ = incr count
             end in
             let%lwt () = Target.execute instance (module Util.Dummy_dispatcher (D)) in
             Lwt.return @@ assert_ok (!D.count = 0) ) ]
