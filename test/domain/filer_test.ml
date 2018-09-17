open Sxfiler_core
module D = Sxfiler_domain
module N = D.Node
module F = D.Filer

let stat_base =
  D.File_stat.make ~mode:(Int32.of_int 0o775) ~uid:1000 ~gid:1000 ~atime:(Int64.of_int 100)
    ~mtime:(Int64.of_int 1000) ~ctime:(Int64.of_int 10000) ~size:Int64.max_int ~is_directory:true
    ~is_file:false ~is_symlink:true

let node_base = D.Node.make ~full_path:(Path.of_string "foo") ~stat:stat_base ~link_path:None

let testcases =
  [ ( "should be sorted with send order when instance created"
    , `Quick
    , fun () ->
      let node_1 =
        D.Node.make ~id:"id1"
          ~full_path:(Path.of_string ~env:`Unix "/foo")
          ~stat:stat_base ~link_path:None
      and node_2 =
        D.Node.make ~id:"id2"
          ~full_path:(Path.of_string ~env:`Unix "/bar")
          ~stat:stat_base ~link_path:None
      in
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
      let node_1 =
        D.Node.make ~id:"id1"
          ~full_path:(Path.of_string ~env:`Unix "/foo")
          ~stat:stat_base ~link_path:None
      and node_2 =
        D.Node.make ~id:"id2"
          ~full_path:(Path.of_string ~env:`Unix "/bar")
          ~stat:stat_base ~link_path:None
      in
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
      Alcotest.(check @@ of_pp Fmt.nop) "subset" expected F.(filer.nodes) ) ]

let () = Alcotest.run "Filer" [("sorting", testcases)]
