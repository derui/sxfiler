open Sxfiler_core
module D = Sxfiler_domain

let state_for_fixture = Random.State.make_self_init ()

let fixture ?id ?items ?location ?(sort_type = D.Types.Sort_type.Name) () =
  let id = Option.value id ~default:Uuidm.(v4_gen state_for_fixture () |> to_string |> D.File_list.Id.make) in
  let items = Option.value items ~default:[ File_item.fixture () ] in
  let location =
    Option.value location ~default:("/" ^ Helper.random_string state_for_fixture 10) |> Path.of_string |> Result.get_ok
  in
  D.File_list.make ~id ~location ~sort_type |> D.File_list.scan (`Scanned items)
