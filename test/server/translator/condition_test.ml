module D = Sxfiler_domain
module T = Sxfiler_server_translator.Condition

let test_set =
  [
    ( "can translate to/from domain",
      `Quick,
      fun () ->
        let data = D.Condition.of_list [ "completing" ] in
        Alcotest.(check @@ of_pp Fmt.nop) "domain" data (T.to_domain @@ T.of_domain data) );
  ]
