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

let test_set =
  [ Alcotest_lwt.test_case "return current configuration" `Quick (fun _ () ->
        let configuration = D.Configuration.default in
        let module CR = struct
          let data = ref configuration
          let resolve () = Lwt.return configuration

          let store v =
            data := v ;
            Lwt.return_unit
        end in
        let module Usecase = U.Configuration.Get.Make (CR) in
        let%lwt result = Usecase.execute () in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "renew filer" (Ok !CR.data) result ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "return current configuration" `Quick (fun _ () ->
        let configuration = D.Configuration.default in
        let module CR = struct
          let data = ref None
          let resolve () = assert false

          let store v =
            data := Some v ;
            Lwt.return_unit
        end in
        let module Usecase = U.Configuration.Store.Make (CR) in
        let%lwt _ = Usecase.execute configuration in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "stored" (Ok !CR.data) (Ok (Some configuration)) ;
        Lwt.return_unit ) ]
