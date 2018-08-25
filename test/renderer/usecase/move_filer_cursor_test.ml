open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase

let suite () =
  "Move filer cursor use case" >::: [
    "should send message to move cursor next" >:- (fun () ->
        let module Target = U.Move_filer_cursor in
        let instance = Target.create `Next in
        let module D = struct
          let message_ = ref None
          let dispatch m = message_ := Some m
        end in

        let%lwt () = Target.execute instance (module Util.Dummy_dispatcher(D)) in
        Lwt.return @@ assert_ok (!D.message_ = Some (C.Message.Move_cursor_to_next))
      );
    "should send message to move cursor previous" >:- (fun () ->
        let module Target = U.Move_filer_cursor in
        let instance = Target.create `Prev in
        let module D = struct
          let message_ = ref None
          let dispatch m = message_ := Some m
        end in

        let%lwt () = Target.execute instance (module Util.Dummy_dispatcher(D)) in
        Lwt.return @@ assert_ok (!D.message_ = Some (C.Message.Move_cursor_to_prev))
      );
  ]
