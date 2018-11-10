module C = Sxfiler_domain.Completion
module CM = Sxfiler_bin_lib.Migemo_completer

let test_set =
  [ ( "can return candidates from collection with input and migemo"
    , `Quick
    , fun () ->
      let module Data = struct
        type t =
          { data : string
          ; index : int }

        let to_string {data; _} = data
      end in
      let collection =
        [ {Data.data = "foo"; index = 0}
        ; {Data.data = "foobar"; index = 1}
        ; {Data.data = "bar"; index = 2}
        ; {Data.data = "barfoo"; index = 3} ]
      in
      let migemo_dict =
        match Migemocaml.Dict_tree.load_dict "dict_file.txt" with
        | None -> Alcotest.fail "Not found dict"
        | Some v -> v
      in
      let migemo = Migemocaml.Migemo.make ~dict:migemo_dict () in
      let module I = (val CM.make ~migemo) in
      let result = I.(Completer.read this ~input:"foo" ~collection ~stringify:(module Data)) in
      let expect =
        [ {C.Candidate.start = 0; length = 3; value = {Data.data = "foo"; index = 0}}
        ; {C.Candidate.start = 0; length = 3; value = {Data.data = "foobar"; index = 1}}
        ; {C.Candidate.start = 3; length = 6; value = {Data.data = "barfoo"; index = 3}} ]
      in
      Alcotest.(check @@ list (of_pp Fmt.nop)) "completed" expect result ) ]
