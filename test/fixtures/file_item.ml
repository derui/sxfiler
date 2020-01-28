module D = Sxfiler_domain
open Sxfiler_core

let state_for_fixture = Random.State.make_self_init ()

(* make fixture for node *)
let fixture ?full_path ?stat () =
  let stat = Option.value stat ~default:(File_stat.file_fixture ()) in
  let full_path =
    Option.value full_path ~default:(Helper.random_string state_for_fixture 10 |> Path.of_string |> Result.get_ok)
  in
  let id = Uuidm.(v4_gen state_for_fixture () |> to_string) |> D.File_item.Id.make in
  D.File_item.make ~id ~full_path ~stat
