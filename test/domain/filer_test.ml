open Sxfiler_core
module D = Sxfiler_domain
module N = D.Node
module F = D.Filer
module TF = Test_fixtures

let subset_test =
  [ ( "should return subset of nodes that includes only specified ids"
    , `Quick
    , fun () ->
      let stat = TF.File_stat.fixture () in
      let nodes =
        [ TF.Node.fixture ~full_path:Path.(of_string "foo") stat
        ; TF.Node.fixture ~full_path:Path.(of_string "bar") stat
        ; TF.Node.fixture ~full_path:Path.(of_string "baz") stat
        ; TF.Node.fixture ~full_path:Path.(of_string "foobar") stat ]
      in
      let filer =
        F.make ~id:"id" ~location:(Path.of_string "/") ~nodes
          ~history:D.Location_history.(make ())
          ~sort_order:D.Types.Sort_type.Name
      in
      let node index = List.nth nodes index in
      let subset, _ = F.node_subset filer ~ids:[(node 1).id; (node 2).id] in
      Alcotest.(check @@ slist (of_pp D.Node.pp) @@ fun v1 v2 -> compare v1.D.Node.id v2.id)
        "subset" subset [node 1; node 2] )
  ; ( "should return rest of ids that includes only which not found in filer"
    , `Quick
    , fun () ->
      let stat = TF.File_stat.fixture () in
      let nodes =
        [ TF.Node.fixture ~full_path:Path.(of_string "foo") stat
        ; TF.Node.fixture ~full_path:Path.(of_string "bar") stat
        ; TF.Node.fixture ~full_path:Path.(of_string "baz") stat
        ; TF.Node.fixture ~full_path:Path.(of_string "foobar") stat ]
      in
      let filer =
        F.make ~id:"id" ~location:(Path.of_string "/") ~nodes
          ~history:D.Location_history.(make ())
          ~sort_order:D.Types.Sort_type.Name
      in
      let node index = List.nth nodes index in
      let subset, rest = F.node_subset filer ~ids:[(node 1).id; (node 2).id; "not found"] in
      Alcotest.(check @@ slist (of_pp D.Node.pp) @@ fun v1 v2 -> compare v1.D.Node.id v2.id)
        "subset" subset [node 1; node 2] ;
      Alcotest.(check @@ list @@ string) "rest" rest ["not found"] ) ]

let test_set =
  let stat = TF.File_stat.fixture () in
  let node_1 = TF.Node.fixture ~full_path:(Path.of_string ~env:`Unix "/foo") stat
  and node_2 = TF.Node.fixture ~full_path:(Path.of_string ~env:`Unix "/bar") stat in
  subset_test
  @ [ ( "should be sorted with send order when instance created"
      , `Quick
      , fun () ->
        let data = [node_1; node_2] in
        let expected = [node_2; node_1] in
        let data =
          F.make ~id:"id" ~location:(Path.of_string "/") ~nodes:data
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name
        in
        Alcotest.(check @@ of_pp Fmt.nop) "subset" expected F.(data.nodes) )
    ; ( "should be sorted when moved filer's location"
      , `Quick
      , fun () ->
        let expected = [node_2; node_1] in
        let filer =
          F.make ~id:"id" ~location:(Path.of_string "/") ~nodes:[]
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name
        in
        let data = [node_1; node_2] in
        let filer =
          F.move_location filer ~location:(Path.of_string "/new") ~nodes:data
            ( module struct
              let unixtime () = Int64.zero
            end )
        in
        Alcotest.(check @@ of_pp Fmt.nop) "subset" expected F.(filer.nodes) )
    ; ( "should be able to select nodes"
      , `Quick
      , fun () ->
        let filer =
          F.make ~id:"id" ~location:(Path.of_string "/") ~nodes:[node_1; node_2]
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name
        in
        let filer = F.select_nodes filer ~ids:["id1"] in
        Alcotest.(check @@ list @@ string) "subset" ["id1"] F.(filer.selected_nodes) )
    ; ( "should be able to deselect nodes"
      , `Quick
      , fun () ->
        let filer =
          F.make ~id:"id" ~location:(Path.of_string "/") ~nodes:[node_1; node_2]
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name
        in
        let filer = F.select_nodes filer ~ids:["id1"; "id2"] |> F.deselect_nodes ~ids:["id2"] in
        Alcotest.(check @@ list @@ string) "subset" ["id1"] F.(filer.selected_nodes) ) ]
