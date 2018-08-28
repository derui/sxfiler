open Mocha_of_ocaml
open Mocha_of_ocaml_async
open Sxfiler_core
module C = Sxfiler_renderer_core
module T = Sxfiler_rpc.Types
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase

let () =
  "Move filer to parent usecase" >::: [
    "should return filer when make operation returns no error" >:- (fun () ->
        let expected = {
          T.Filer.id = "foo";
          location = "loc";
          nodes = [];
          history = T.Location_history.{records = []; max_records = 100}
        } in

        let module S : S.Filer.S = struct
          let make _ = assert false
          let get _ = assert false
          let move_parent _ = Lwt.return expected
        end in

        let module Target = U.Move_filer_to_parent.Make(S) in
        let instance = Target.create `Left in
        let module D = struct
          let message_ = ref None
          let dispatch m = message_ := Some m
        end in

        let%lwt () = Target.execute instance (module Util.Dummy_dispatcher(D)) in
        Lwt.return @@ assert_ok (!D.message_ = Option.some @@ C.Message.Update_filer (`Left, expected))
      );

    "should be send error message when error occurred" >:- (fun () ->

        let module S : S.Filer.S = struct
          let make _ = assert false
          let get _ = assert false
          let move_parent _ = Lwt.fail Error.(create "foo" |> to_exn)
        end in

        let module Target = U.Move_filer_to_parent.Make(S) in
        let instance = Target.create `Left in
        let module D = struct
          let message_ = ref None
          let dispatch m = message_ := Some m
        end in

        let%lwt () = Target.execute instance (module Util.Dummy_dispatcher(D)) in
        Lwt.return @@ assert_ok (match !D.message_ with
            | Some C.Message.Raise_error _ -> true
            | _ -> false)
      );
  ]
