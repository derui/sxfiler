module D = Sxfiler_domain
module T = Sxfiler_translator.Keymap
module F = Test_fixtures

let test_set =
  [
    Alcotest_lwt.test_case_sync "can translate to/from domain" `Quick (fun () ->
        let binding = D.Keymap.Binding.make ~context:D.Context.empty ~key:(Sxfiler_kbd.make "k") in
        let data = D.Keymap.(empty |> add ~binding ~action:(Action.make "action")) in
        Alcotest.(check @@ result F.Testable.keymap @@ of_pp T.pp_error)
          "domain" (Ok data)
          (T.to_domain @@ T.of_domain data));
  ]
