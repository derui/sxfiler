open Sxfiler_core
module B = Sxfiler_domain.Bookmark

let test_set =
  [ ( "should compare between bookmark ids"
    , `Quick
    , fun () ->
        let id = Uuidm.v4_gen (Random.get_state ()) () in
        let copied_id1 = Uuidm.to_string id |> Uuidm.of_string |> Option.get_exn
        and copied_id2 = Uuidm.to_string id |> Uuidm.of_string |> Option.get_exn in
        Alcotest.(check bool) "subset" true B.(equal_id copied_id1 copied_id2) ) ]
