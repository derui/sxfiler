open Sxfiler_core
module D = Sxfiler_domain
module T = Sxfiler_server_translator.Bookmark

let data =
  D.Bookmark.make
    ~id:(Uuidm.v4_gen (Random.State.make [||]) ())
    ~path:Path.(of_string "/foo")
    ~order:1

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () -> Alcotest.(check @@ of_pp Fmt.nop) "domain" data (T.to_domain @@ T.of_domain data)
    )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
        let data = T.of_domain data in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "yojson" (Ok data)
          (T.of_json @@ T.to_json data) ) ]
