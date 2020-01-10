module D = Sxfiler_domain
module Tr = Sxfiler_server_translator.Task_interaction

let state = Random.State.make [| 0 |]

let test_set =
  [
    ( "can translate to/from domain",
      `Quick,
      fun () ->
        let task_id = Uuidm.v4_gen state () in
        let data =
          { D.Task_interaction.Suggestion.task_id; suggestions = [ Overwrite ]; item_name = "item" }
        in
        Alcotest.(check @@ of_pp Fmt.nop) "domain" data Tr.Suggestion.(of_domain data |> to_domain)
    );
  ]
