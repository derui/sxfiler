open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator

module Factory = D.Filer.Factory.Make (struct
  type id = D.Filer.id

  let state = Random.get_state ()
  let generate () = Uuidm.v4_gen state ()
end)

let data =
  Factory.create ~name:"id"
    ~file_list:Test_fixtures.(File_list.empty_list (Path.of_string "/bar"))
    ~sort_order:D.Types.Sort_type.Date

let test_set =
  [
    ( "can translate to/from domain",
      `Quick,
      fun () ->
        Alcotest.(check @@ of_pp D.Filer.pp) "domain" data Tr.Filer.(to_domain @@ of_domain data) );
  ]
