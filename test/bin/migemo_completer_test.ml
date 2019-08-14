module C = Sxfiler_domain.Completion
module CM = Sxfiler_bin_lib.Migemo_completer

let test_set =
  [ ( "can return candidates from collection with input and migemo"
    , `Quick
    , fun () ->
        let collection =
          [ {C.Item.value = "foo"; id = "0"}
          ; {C.Item.value = "foobar"; id = "1"}
          ; {C.Item.value = "bar"; id = "2"}
          ; {C.Item.value = "barfoo"; id = "3"} ]
        in
        let migemo_dict =
          match Migemocaml.Dict_tree.load_dict "dict_file.txt" with
          | None -> Alcotest.fail "Not found dict"
          | Some v -> v
        in
        let migemo = Migemocaml.Migemo.make ~dict:migemo_dict () in
        let module I = (val CM.make ~migemo) in
        let result = I.(Completer.read this ~input:"foo" ~collection) in
        let expect =
          [ {C.Candidate.start = 0; length = 3; value = List.nth collection 0}
          ; {C.Candidate.start = 0; length = 3; value = List.nth collection 1}
          ; {C.Candidate.start = 3; length = 3; value = List.nth collection 3} ]
        in
        Alcotest.(check @@ list (of_pp C.Candidate.pp)) "completed" expect result ) ]
