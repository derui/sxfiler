module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator

module Dummy : D.Plan.Executor = struct
  let do_plan _ = Lwt.return_ok ()
end

let test_set =
  [ Alcotest_lwt.test_case "make plan to move nodes" `Quick (fun _ () ->
        let id = Uuidm.v4_gen (Random.get_state ()) () |> Uuidm.to_bytes in
        let plan = D.Plan.make ~id ~executor:(module Dummy) ~target_nodes:[] in
        let module Usecase = struct
          include U.Plan.Filer.Make_move_plan.Type

          let execute {source; dest; node_ids} =
            Alcotest.(check string) "source" source "from" ;
            Alcotest.(check string) "dest" dest "to" ;
            Alcotest.(check @@ list string) "node_ids" node_ids ["id"] ;
            Lwt.return_ok plan
        end in
        let module Gateway = G.Plan.Filer.Make_move_plan.Make (Usecase) in
        let%lwt res = Gateway.handle {source = "from"; node_ids = ["id"]; dest = "to"} in
        Alcotest.(check @@ of_pp T.Plan.pp) "created" (T.Plan.of_domain plan) res ;
        Lwt.return_unit ) ]
