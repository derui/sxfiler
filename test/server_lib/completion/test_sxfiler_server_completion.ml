module T = Sxfiler_types.Completion
module C = Sxfiler_server_completion

let suite = [
  "can return candidates from collection with input and migemo", `Quick,
  (fun () ->
     let module Data = struct
       type t = {
         data: string;
         index: int;
       }

       let to_string {data} = data
     end in
     let collection = [
       {Data.data = "foo"; index = 0};
       {Data.data = "foobar"; index = 1};
       {Data.data = "bar"; index = 2};
       {Data.data = "barfoo"; index = 3};
     ] in
     let migemo_dict = match Migemocaml.Dict_tree.load_dict "dict_file.txt" with
       | None -> Alcotest.fail "Not found dict"
       | Some v -> v
     in
     let migemo = Migemocaml.Migemo.make ~dict:migemo_dict () in
     let module I = (val C.Completer.make ~migemo) in
     let result = I.(Completer.read instance ~input:"foo"
                       ~collection
                       ~stringify:(module Data)) in
     let expect = [
       {T.Candidate.start = 0; length = 3; value =  {Data.data = "foo"; index = 0}};
       {T.Candidate.start = 0; length = 3; value =  {Data.data = "foobar"; index = 1}};
       {T.Candidate.start = 3; length = 6; value =  {Data.data = "barfoo"; index = 3}};
     ] in
     Alcotest.(check @@ list (of_pp Fmt.nop)) "completed" expect result
  );
]

let () =
  Alcotest.run "Completer" [
    "suite", suite;
  ]
