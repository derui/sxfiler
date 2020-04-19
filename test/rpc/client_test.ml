open Sxfiler_core
module D = Sxfiler_domain
module R = Sxfiler_rpc
module G = Sxfiler_generated
module I = Sxfiler_infrastructure
module Tr = Sxfiler_translator
module Pb = Ocaml_protoc_plugin
module W = Websocket

let test_set =
  let module Gen = struct
    type id = string

    let generate () = "request-response"
  end in
  [
    Alcotest_lwt.test_case "call command and return valid response" `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let expected = { G.Filer.CopyUserDecisionResponse.action = OVERWRITE; new_name = "" } in
        let push, (module Actor) = Dummy_actor.make () in
        let module C = (val R.Client.make (module Gen) (module Actor)) in
        let request = { G.Filer.CopyUserDecisionRequest.item = Tr.File_item.of_domain item |> Option.some } in
        let waiter = Actor.(Actor.start instance) in
        let ret = C.(Client.call instance R.Client_command.Filer.copy_interaction request) in
        let payload =
          {
            G.Service.Response.id = "request-response";
            payload = G.Filer.CopyUserDecisionResponse.to_proto expected |> Pb.Writer.contents |> Bytes.of_string;
            status = G.Service.Status.SUCCESS;
            error = None;
          }
          |> G.Service.Response.to_proto |> Pb.Writer.contents
        in
        push (W.Frame.create ~opcode:W.Frame.Opcode.Binary ~content:payload ());
        Actor.(Actor.stop instance);
        let%lwt ret = ret in
        let%lwt () = waiter in
        Alcotest.(check & result (of_pp G.Filer.CopyUserDecisionResponse.pp) & of_pp Fmt.nop) "event" (Ok expected) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "call command and return error" `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let push, (module Actor) = Dummy_actor.make () in
        let module C = (val R.Client.make (module Gen) (module Actor)) in
        let request = { G.Filer.CopyUserDecisionRequest.item = Tr.File_item.of_domain item |> Option.some } in
        let waiter = Actor.(Actor.start instance) in
        let ret = C.(Client.call instance R.Client_command.Filer.copy_interaction request) in
        let payload =
          {
            G.Service.Response.id = "request-response";
            payload = Bytes.of_string "a";
            status = G.Service.Status.SUCCESS;
            error = None;
          }
          |> G.Service.Response.to_proto |> Pb.Writer.contents
        in
        push (W.Frame.create ~opcode:W.Frame.Opcode.Binary ~content:payload ());
        let%lwt ret = ret in
        Actor.(Actor.stop instance);
        let%lwt () = waiter in
        Alcotest.(check & result (of_pp G.Filer.CopyUserDecisionResponse.pp) & of_pp Fmt.nop)
          "event"
          (Error (`Pb_error `Premature_end_of_input))
          ret;
        Lwt.return_unit);
  ]
