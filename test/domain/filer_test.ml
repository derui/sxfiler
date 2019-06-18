open Sxfiler_core
module D = Sxfiler_domain
module N = D.Node
module F = D.Filer
module TF = Test_fixtures

module Clock = struct
  let unixtime () = Int64.min_int
end

module Factory = D.Filer.Factory.Make (struct
    type id = Uuidm.t

    let state = Random.get_state ()
    let generate () = Uuidm.v4_gen state ()
  end)

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
      let file_tree = D.File_tree.make ~location:(Path.of_string "/") ~nodes in
      let filer = Factory.create ~name:"id" ~file_tree ~sort_order:D.Types.Sort_type.Name in
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
      let file_tree = D.File_tree.make ~location:(Path.of_string "/") ~nodes in
      let filer = Factory.create ~name:"id" ~file_tree ~sort_order:D.Types.Sort_type.Name in
      let node index = List.nth nodes index in
      let subset, rest = F.node_subset filer ~ids:[(node 1).id; (node 2).id; "not found"] in
      Alcotest.(check @@ slist (of_pp D.Node.pp) @@ fun v1 v2 -> compare v1.D.Node.id v2.id)
        "subset" subset [node 1; node 2] ;
      Alcotest.(check @@ list @@ string) "rest" rest ["not found"] ) ]

let update_test_set =
  let stat = TF.File_stat.fixture () in
  let node_1 = TF.Node.fixture ~full_path:(Path.of_string ~env:`Unix "/foo") stat
  and node_2 = TF.Node.fixture ~full_path:(Path.of_string ~env:`Unix "/bar") stat in
  [ ( "hold marked_nodes if the new file_tree has same location"
    , `Quick
    , fun () ->
      let data = [node_1] in
      let expected = [node_2; node_1] in
      let file_tree = D.File_tree.make ~location:(Path.of_string "/") ~nodes:data in
      let file_tree' = D.File_tree.make ~location:(Path.of_string "/") ~nodes:expected in
      let data =
        Factory.create ~name:"id" ~file_tree ~sort_order:D.Types.Sort_type.Name
        |> D.Filer.add_mark ~ids:[node_1.id]
      in
      let data = D.Filer.move_location data ~file_tree:file_tree' (module Clock) in
      let expected = D.Filer.Node_id_set.(empty |> add node_1.id) in
      Alcotest.(check @@ of_pp D.Filer.Node_id_set.pp)
        "same node id" expected
        F.(data.marked_nodes) ) ]

let test_set =
  let stat = TF.File_stat.fixture () in
  let node_1 = TF.Node.fixture ~full_path:(Path.of_string ~env:`Unix "/foo") stat
  and node_2 = TF.Node.fixture ~full_path:(Path.of_string ~env:`Unix "/bar") stat in
  subset_test @ update_test_set
  @ [ ( "should be sorted with send order when instance created by Factory"
      , `Quick
      , fun () ->
        let data = [node_1; node_2] in
        let expected = [node_2; node_1] in
        let file_tree = D.File_tree.make ~location:(Path.of_string "/") ~nodes:data in
        let data = Factory.create ~name:"id" ~file_tree ~sort_order:D.Types.Sort_type.Name in
        Alcotest.(check @@ list @@ TF.Testable.node) "subset" expected F.(data.file_tree.nodes)
      )
    ; ( "should be sorted when moved filer's location"
      , `Quick
      , fun () ->
        let data = [node_1; node_2] in
        let expected = [node_2; node_1] in
        let file_tree = D.File_tree.make ~location:(Path.of_string "/") ~nodes:data in
        let filer = Factory.create ~name:"id" ~file_tree ~sort_order:D.Types.Sort_type.Name in
        let filer =
          F.move_location filer
            ~file_tree:D.File_tree.(make ~location:(Path.of_string "/new") ~nodes:data)
            ( module struct
              let unixtime () = Int64.zero
            end )
        in
        Alcotest.(check @@ list @@ TF.Testable.node) "subset" expected F.(filer.file_tree.nodes) ;
        Alcotest.(check @@ of_pp Path.pp)
          "location" (Path.of_string "/new")
          F.(filer.file_tree.location) )
    ; ( "should be able to select nodes"
      , `Quick
      , fun () ->
        let file_tree =
          D.File_tree.make ~location:(Path.of_string "/") ~nodes:[node_1; node_2]
        in
        let filer = Factory.create ~name:"id" ~file_tree ~sort_order:D.Types.Sort_type.Name in
        let filer = F.add_mark filer ~ids:["id1"] in
        let expected = F.Node_id_set.of_list ["id1"] in
        Alcotest.(check @@ of_pp F.Node_id_set.pp) "subset" expected F.(filer.marked_nodes) )
    ; ( "should be able to remove nodes from selected these"
      , `Quick
      , fun () ->
        let file_tree =
          D.File_tree.make ~location:(Path.of_string "/") ~nodes:[node_1; node_2]
        in
        let filer = Factory.create ~name:"id" ~file_tree ~sort_order:D.Types.Sort_type.Name in
        let filer = F.add_mark filer ~ids:["id1"; "id2"] |> F.remove_mark ~ids:["id2"] in
        let expected = F.Node_id_set.of_list ["id1"] in
        Alcotest.(check @@ of_pp F.Node_id_set.pp) "subset" expected F.(filer.marked_nodes) ) ]
