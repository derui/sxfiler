module D = Sxfiler_domain
module S = Sxfiler_server
module U = Sxfiler_usecase
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator

module Dummy_system = struct
  let getcwd () = "/foo"
end

let random_state = Random.State.make [|0|]
let task_id = Uuidm.v4_gen random_state ()

let test_set =
  [ Alcotest_lwt.test_case "execute use case with the parameters" `Quick (fun _ () ->
        let spy, f = Spy.wrap (fun _ -> Lwt.return_ok ()) in
        let module Usecase = struct
          include U.Task.Send_interaction.Type

          let execute = f
        end in
        let module Gateway = G.Task.Send_interaction.Make (Usecase) in
        let%lwt () =
          Gateway.handle
            { task_id = Uuidm.to_string task_id
            ; payload = T.Task_interaction.Interaction.Yes_no false }
        in
        Alcotest.(check @@ list @@ of_pp Fmt.nop)
          "called"
          [{Usecase.task_id; interaction = D.Task.Interaction.Yes_no false}]
          (Spy.Wrap.called_args spy) ;
        Lwt.return_unit ) ]
