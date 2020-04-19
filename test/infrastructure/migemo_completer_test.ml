module C = Sxfiler_domain.Completer
module CM = Sxfiler_infrastructure.Migemo_completer

let test_set =
  [
    Alcotest_lwt.test_case_sync "can return candidates from collection with input and migemo" `Quick (fun () ->
        let collection =
          [
            C.Item.make ~id:"0" ~value:"foo";
            C.Item.make ~id:"1" ~value:"foobar";
            C.Item.make ~id:"2" ~value:"bar";
            C.Item.make ~id:"3" ~value:"barfoo";
          ]
        in
        let migemo_dict =
          match Migemocaml.Dict_tree.load_dict (Filename.concat "data_real" "dict_file.txt") with
          | None   -> Alcotest.fail "Not found dict"
          | Some v -> v
        in
        let migemo = Migemocaml.Migemo.make ~dict:migemo_dict () in
        let module I = (val CM.make ~migemo) in
        let result = I.(Completer.read this ~input:"foo" ~collection) in
        let expect =
          [
            C.Candidate.make ~start:0 ~length:3 ~value:(List.nth collection 0);
            C.Candidate.make ~start:0 ~length:3 ~value:(List.nth collection 1);
            C.Candidate.make ~start:3 ~length:3 ~value:(List.nth collection 3);
          ]
        in
        Alcotest.(check @@ list (of_pp C.Candidate.pp)) "completed" expect result);
  ]
