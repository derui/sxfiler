module D = Sxfiler_domain
module Tr = Sxfiler_server_translator.Task_interaction

let state = Random.State.make [|0|]

module F = D.Task_interaction.Factory.Make (struct
    type id = Uuidm.t

    let generate () = Uuidm.v4_gen state ()
  end)

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      let task_id = Uuidm.v4_gen state () in
      let data = F.create ~task_id ~accept_interactions:[Yes_no] in
      Alcotest.(check @@ of_pp Fmt.nop) "domain" data Tr.(of_domain data |> to_domain) )
  ; ( "can translate to/from json"
    , `Quick
    , fun () ->
      let task_id = Uuidm.v4_gen state () in
      let data = F.create ~task_id ~accept_interactions:[String] |> Tr.of_domain in
      Alcotest.(check @@ result (of_pp Tr.pp) (of_pp Fmt.nop))
        "json" (Ok data)
        Tr.(of_json @@ to_json data) ) ]
