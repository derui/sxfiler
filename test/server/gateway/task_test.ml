module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module Gen = Sxfiler_server_generated

module Dummy_system = struct
  let getcwd () = "/foo"
end

let random_state = Random.State.make [| 0 |]
let task_id = Uuidm.v4_gen random_state ()

let test_set =
  [
    Alcotest_lwt.test_case "execute use case with the parameters" `Quick (fun _ () ->
        let spy, f = Spy.wrap (fun _ -> Lwt.return_ok ()) in
        let module Usecase = struct
          include U.Task.Send_reply.Type

          let execute = f
        end in
        let module Gateway = G.Task.Send_reply.Make (Usecase) in
        let%lwt _ =
          Gateway.handle
            (Some
               {
                 taskId = Uuidm.to_string task_id;
                 reply = `Overwrite true;
                 type' = Gen.Task.ReplyType.Overwrite;
               })
        in
        Alcotest.(check @@ list @@ of_pp Fmt.nop)
          "called"
          [ { D.Task_interaction.Reply.task_id; reply = D.Task_interaction.Reply.Overwrite true } ]
          (Spy.Wrap.called_args spy);
        Lwt.return_unit);
  ]
