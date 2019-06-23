open Sxfiler_core
module D = Sxfiler_domain
module N = D.File_item
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

let update_test_set =
  let stat = TF.File_stat.fixture () in
  let item_1 = TF.File_item.fixture ~full_path:(Path.of_string ~env:`Unix "/foo") stat
  and item_2 = TF.File_item.fixture ~full_path:(Path.of_string ~env:`Unix "/bar") stat in
  [ ( "hold marked_nodes if the new file_list has same location"
    , `Quick
    , fun () ->
      let data = [item_1] in
      let expected = [item_2; item_1] in
      let file_list = D.File_list.make ~location:(Path.of_string "/") ~items:data () in
      let file_list' = D.File_list.make ~location:(Path.of_string "/") ~items:expected () in
      let data =
        Factory.create ~name:"id" ~file_list ~sort_order:D.Types.Sort_type.Name
        |> D.Filer.add_mark ~ids:[item_1.id]
      in
      let data = D.Filer.move_location data ~file_list:file_list' (module Clock) in
      let expected = D.Filer.Marked_item_set.(empty |> add item_1.id) in
      Alcotest.(check @@ of_pp D.Filer.Marked_item_set.pp)
        "same item id" expected
        F.(data.marked_items) ) ]

let test_set =
  let stat = TF.File_stat.fixture () in
  let item_1 = TF.File_item.fixture ~full_path:(Path.of_string ~env:`Unix "/foo") stat
  and item_2 = TF.File_item.fixture ~full_path:(Path.of_string ~env:`Unix "/bar") stat in
  update_test_set
  @ [ ( "should be sorted with send order when instance created by Factory"
      , `Quick
      , fun () ->
        let data = [item_1; item_2] in
        let expected = [item_2; item_1] in
        let file_list = D.File_list.make ~location:(Path.of_string "/") ~items:data () in
        let data = Factory.create ~name:"id" ~file_list ~sort_order:D.Types.Sort_type.Name in
        Alcotest.(check @@ list @@ TF.Testable.file_item)
          "subset" expected
          F.(data.file_list.items) )
    ; ( "should be sorted when moved filer's location"
      , `Quick
      , fun () ->
        let data = [item_1; item_2] in
        let expected = [item_2; item_1] in
        let file_list = D.File_list.make ~location:(Path.of_string "/") ~items:data () in
        let filer = Factory.create ~name:"id" ~file_list ~sort_order:D.Types.Sort_type.Name in
        let filer =
          F.move_location filer
            ~file_list:D.File_list.(make ~location:(Path.of_string "/new") ~items:data ())
            ( module struct
              let unixtime () = Int64.zero
            end )
        in
        Alcotest.(check @@ list @@ TF.Testable.file_item)
          "subset" expected
          F.(filer.file_list.items) ;
        Alcotest.(check @@ of_pp Path.pp)
          "location" (Path.of_string "/new")
          F.(filer.file_list.location) )
    ; ( "should be able to select nodes"
      , `Quick
      , fun () ->
        let file_list =
          D.File_list.make ~location:(Path.of_string "/") ~items:[item_1; item_2] ()
        in
        let filer = Factory.create ~name:"id" ~file_list ~sort_order:D.Types.Sort_type.Name in
        let filer = F.add_mark filer ~ids:["id1"] in
        let expected = F.Marked_item_set.of_list ["id1"] in
        Alcotest.(check @@ of_pp F.Marked_item_set.pp) "subset" expected F.(filer.marked_items)
      )
    ; ( "should be able to remove nodes from selected these"
      , `Quick
      , fun () ->
        let file_list =
          D.File_list.make ~location:(Path.of_string "/") ~items:[item_1; item_2] ()
        in
        let filer = Factory.create ~name:"id" ~file_list ~sort_order:D.Types.Sort_type.Name in
        let filer = F.add_mark filer ~ids:["id1"; "id2"] |> F.remove_mark ~ids:["id2"] in
        let expected = F.Marked_item_set.of_list ["id1"] in
        Alcotest.(check @@ of_pp F.Marked_item_set.pp) "subset" expected F.(filer.marked_items)
      ) ]
