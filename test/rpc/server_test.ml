open Sxfiler_core
module R = Sxfiler_rpc
module G = Sxfiler_generated
module I = Sxfiler_infrastructure
module Pb = Ocaml_protoc_plugin
module W = Websocket

let test_set =
  [
    Alcotest_lwt.test_case "call mapping when it contains mappings" `Quick (fun _ () ->
        let push, (module Actor) = Dummy_actor.make () in
        let module S = (val R.Server.make (module Actor)) in
        let request =
          {
            G.Service.Request.command = G.Service.Command.FILER_INITIALIZE;
            payload =
              { G.Filer.InitializeRequest.left_location = "left"; right_location = "right" }
              |> G.Filer.InitializeRequest.to_proto |> Pb.Writer.contents |> Bytes.of_string;
            id = "id";
          }
        in
        let handled, wakener = Lwt.wait () in
        let mappings =
          [
            ( G.Service.Command.FILER_INITIALIZE,
              fun v ->
                Alcotest.(check & of_pp G.Service.Request.pp) "request" request v;
                Lwt.wakeup wakener ();
                Lwt.return
                  ( {
                      G.Service.Response.id = "id";
                      payload = Bytes.empty;
                      status = G.Service.Status.SUCCESS;
                      error = None;
                    },
                    [] ) );
          ]
        in
        let actor_waiter = Actor.(Actor.start instance) in
        let waiter = S.(Server.start instance ~mappings ~post_event:(fun _ -> Lwt.return_unit)) in
        push
          (W.Frame.create ~opcode:W.Frame.Opcode.Binary
             ~content:(request |> G.Service.Request.to_proto |> Pb.Writer.contents)
             ());
        S.(Server.stop instance);
        Actor.(Actor.stop instance);
        let%lwt () = Lwt.join [ waiter; actor_waiter; handled ] in
        Lwt.return_unit);
  ]
