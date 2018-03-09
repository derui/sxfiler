open Mocha_of_ocaml
module C = Sxfiler_completer

let () =
  "Completer" >::: [
    "can match string as forward exact match " >:: (fun () ->
        let module Data = struct
          type t = {
            data: string;
            index: int;
          }

          let to_string {data} = data
        end in
        let candidates = [
          {Data.data = "foo"; index = 0};
          {Data.data = "foobar"; index = 1};
          {Data.data = "bar"; index = 2};
          {Data.data = "barfoo"; index = 3};
        ] in
        let result = C.Completer.(complete ~input:"foo"
                                    ~match_type:Forward_exact_match
                                    ~candidates
                                    ~stringify:(module Data)) in
        let result = List.map (fun {Data.data;_} -> Js.string data) result in

        assert_strict_eq [Js.string "foo";Js.string "foobar"] result
      );
    "can partially match with candidate" >:: (fun () ->
        let module Data = struct
          type t = {
            data: string;
            index: int;
          }

          let to_string {data} = data
        end in
        let candidates = [
          {Data.data = "foobar"; index = 0};
          {Data.data = "foobarbaz"; index = 1};
          {Data.data = "barbazfoo"; index = 2};
          {Data.data = "bazf"; index = 3};
        ] in
        let result = C.Completer.(complete ~input:"baz"
                                    ~match_type:Partial_match
                                    ~candidates
                                    ~stringify:(module Data)) in
        let result = List.map (fun {Data.data;_} -> Js.string data) result in

        assert_strict_eq [Js.string "foobarbaz";Js.string "barbazfoo";Js.string "bazf"] result
      )
  ]
