open Sxfiler_core
module D = Sxfiler_domain

let state_for_fixture = Random.State.make_self_init ()

(* make fixture for node *)
let fixture ?(full_path = Path.of_string "foo") ?(link_path = None) stat =
  let id = Uuidm.(v4_gen state_for_fixture () |> to_string) in
  D.File_item.make ~id ~full_path ~stat ~link_path
