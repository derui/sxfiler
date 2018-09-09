open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

let node =
  { D.Node.id = "id"
  ; full_path = Path.of_string "/bar"
  ; stat =
      D.File_stat.make ~mode:(Int32.of_int 0o775) ~uid:1000 ~gid:1000 ~atime:(Int64.of_int 100)
        ~mtime:(Int64.of_int 1000) ~ctime:(Int64.of_int 10000) ~size:Int64.max_int
        ~is_directory:true ~is_file:false ~is_symlink:true
  ; link_path = Some "/foo" }

let testcases =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      let actual =
        { D.Plan.workbench_id = Uuidm.v4_gen (Random.get_state ()) ()
        ; source =
            [ {operation = D.Plan.Operation.Append; node}
            ; {operation = D.Plan.Operation.Delete; node} ]
        ; dest = [{operation = D.Plan.Operation.Remained; node}] }
      in
      let expected =
        { T.Plan.workbench_id = Uuidm.to_string actual.workbench_id
        ; source =
            [ {operation = T.Plan.Operation.Append; node = Tr.Node.of_domain node}
            ; {operation = T.Plan.Operation.Delete; node = Tr.Node.of_domain node} ]
        ; dest = [{operation = T.Plan.Operation.Remained; node = Tr.Node.of_domain node}] }
      in
      Alcotest.(check @@ of_pp Fmt.nop) "domain" expected (Tr.Plan.of_domain actual) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data =
        Tr.Plan.of_domain
          { D.Plan.workbench_id = Uuidm.v4_gen (Random.get_state ()) ()
          ; source =
              [ {operation = D.Plan.Operation.Append; node}
              ; {operation = D.Plan.Operation.Delete; node} ]
          ; dest = [{operation = D.Plan.Operation.Remained; node}] }
      in
      Alcotest.(check @@ result (of_pp Fmt.nop) string)
        "yojson" (Ok data)
        Fun.(Tr.Plan.to_yojson data |> Tr.Plan.of_yojson) ) ]

let suite = [("plan", testcases)]
