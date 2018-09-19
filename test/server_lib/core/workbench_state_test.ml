open Sxfiler_core
module D = Sxfiler_domain
module C = Sxfiler_server_core

let filer =
  D.Filer.make ~id:"foo" ~location:(Path.of_string "/var") ~nodes:[]
    ~sort_order:D.Types.Sort_type.Date ~history:(D.Location_history.make ())

module Factory = struct
  let id = Uuidm.v4_gen (Random.get_state ()) ()
  let make env = {D.Workbench.env; id; corrections = []}
end

let testcases =
  [ ( "can add and find workbench"
    , `Quick
    , fun () ->
      let module W = C.Workbench_state in
      let empty = W.empty in
      let data = Factory.make {source = filer; dest = filer; nodes = []} in
      let state = W.add ~value:data empty in
      let data' = W.find ~id:Factory.id state in
      Alcotest.(check @@ option @@ of_pp Fmt.nop) "same data" (Some data) data' )
  ; ( "can remove from state"
    , `Quick
    , fun () ->
      let module W = C.Workbench_state in
      let empty = W.empty in
      let data = Factory.make {source = filer; dest = filer; nodes = []} in
      let state = W.add ~value:data empty in
      let state = W.remove ~id:Factory.id state in
      let data' = W.find ~id:Factory.id state in
      Alcotest.(check @@ option @@ of_pp Fmt.nop) "none" None data' )
  ; ( "overwrite value if having same id"
    , `Quick
    , fun () ->
      let module W = C.Workbench_state in
      let node =
        { D.Node.id = "id"
        ; full_path = Path.of_string "/bar"
        ; stat =
            D.File_stat.make ~mode:(Int32.of_int 0o775) ~uid:1000 ~gid:1000
              ~atime:(Int64.of_int 100) ~mtime:(Int64.of_int 1000) ~ctime:(Int64.of_int 10000)
              ~size:Int64.max_int ~is_directory:true ~is_file:false ~is_symlink:true
        ; link_path = Some "/foo" }
      in
      let data = Factory.make {source = filer; dest = filer; nodes = []} in
      let data' = Factory.make {source = filer; dest = filer; nodes = [node]} in
      let state = W.add ~value:data W.empty in
      let state = W.add ~value:data' state in
      let actual = Option.(W.find ~id:Factory.id state >|= fun v -> v.env.nodes) in
      Alcotest.(check @@ option @@ of_pp Fmt.nop) "none" (Some data'.env.nodes) actual ) ]

let testcases_for_find =
  [ ( "return None if not found workbench related id"
    , `Quick
    , fun () ->
      let module W = C.Workbench_state in
      let data' = W.find ~id:Factory.id W.empty in
      Alcotest.(check @@ option @@ of_pp Fmt.nop) "none" None data' ) ]

let () =
  Alcotest.run "Workbench backend" [("add and remove", testcases); ("find", testcases_for_find)]
