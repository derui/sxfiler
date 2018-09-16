open Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase

let stat_base =
  D.File_stat.make ~mode:(Int32.of_int 0o775) ~uid:1000 ~gid:1000 ~atime:(Int64.of_int 100)
    ~mtime:(Int64.of_int 1000) ~ctime:(Int64.of_int 10000) ~size:Int64.max_int ~is_directory:false
    ~is_file:true ~is_symlink:true

let dir_stat =
  D.File_stat.make ~mode:(Int32.of_int 0o775) ~uid:1000 ~gid:1000 ~atime:(Int64.of_int 100)
    ~mtime:(Int64.of_int 1000) ~ctime:(Int64.of_int 10000) ~size:Int64.max_int ~is_directory:true
    ~is_file:false ~is_symlink:true

let node_base ~id ?(full_path = Path.of_string "foo") ?(stat = stat_base) ?(link_path = None) () =
  D.Node.make ~id ~full_path ~stat ~link_path

let enter_directory_tests =
  [ Alcotest_lwt.test_case "renewal filer with entered directory" `Quick (fun _ () ->
        let filer =
          D.Filer.make ~id:"foo"
            ~location:Path.(of_string "/bar")
            ~nodes:[node_base ~id:"bar" ~stat:dir_stat ()]
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name
        in
        let module SR = struct
          let data = ref filer
          let resolve _ = Lwt.return_some filer

          let store v =
            data := v ;
            Lwt.return_unit
        end in
        let new_nodes = [node_base ~id:"foo" ()] in
        let module Svc = struct
          let scan _ = Lwt.return new_nodes
        end in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Enter_directory (SR) (Svc) (Clock) in
        let%lwt result = Usecase.execute {name = "foo"; node_id = "bar"} in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "renew filer" (Ok !SR.data) result ;
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "renew location"
          (Ok Path.(of_string "foo"))
          Result.Infix.(result >|= fun v -> v.location) ;
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "renew nodes" (Ok new_nodes)
          Result.Infix.(result >|= fun v -> v.nodes) ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "error when filer not found" `Quick (fun _ () ->
        let module SR = struct
          let resolve _ = Lwt.return_none
          let store _ = assert false
        end in
        let module Svc = struct
          let scan _ = assert false
        end in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Enter_directory (SR) (Svc) (Clock) in
        let%lwt result = Usecase.execute {name = "foo"; node_id = "bar"} in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "not found filer"
          (Error `Not_found_filer)
          result ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "issue error when node not found" `Quick (fun _ () ->
        let filer =
          D.Filer.make ~id:"foo"
            ~location:Path.(of_string "/bar")
            ~nodes:[node_base ~id:"bar" ~stat:dir_stat ()]
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name
        in
        let module SR = struct
          let resolve _ = Lwt.return_some filer
          let store _ = assert false
        end in
        let module Svc = struct
          let scan _ = assert false
        end in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Enter_directory (SR) (Svc) (Clock) in
        let%lwt result = Usecase.execute {name = "foo"; node_id = "not found"} in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "not found node"
          (Error `Not_found_node)
          result ;
        Lwt.return_unit ) ]

let testcases = [("Enter directory", enter_directory_tests)]
