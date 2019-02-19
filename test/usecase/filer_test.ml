open Sxfiler_core
module F = Test_fixtures
module D = Sxfiler_domain
module U = Sxfiler_usecase

let dir_stat = Test_fixtures.File_stat.fixture ~directory:true ()

let test_set =
  let module TF = Test_fixtures in
  [ Alcotest_lwt.test_case "renewal filer with entered directory" `Quick (fun _ () ->
        let file_tree =
          D.File_tree.make ~location:Path.(of_string "/bar") ~nodes:[TF.Node.fixture dir_stat]
        in
        let filer =
          D.Filer.make ~id:"foo" ~file_tree
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name ()
        in
        let module FR =
          (val Test_fixtures.Memory_repository.filer_repository ~initial:[filer] ())
        in
        let new_nodes = [TF.Node.fixture ~full_path:Path.(of_string "new") dir_stat] in
        let module Svc =
          (val TF.Service.location_scanner_service Path.(of_string "/bar") new_nodes)
        in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Enter_directory.Make (FR) (Svc) (Clock) in
        let%lwt result =
          Usecase.execute {name = "foo"; node_id = List.hd file_tree.nodes |> D.Node.id}
        in
        let%lwt data = Lwt.(FR.resolve "foo" >|= Option.get_exn) in
        Alcotest.(check @@ result (of_pp D.Filer.pp) (of_pp Fmt.nop))
          "renew filer" (Ok data) result ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "error when filer not found" `Quick (fun _ () ->
        let module FR = (val TF.Memory_repository.filer_repository ()) in
        let module Svc = struct
          let scan _ = assert false
        end in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Enter_directory.Make (FR) (Svc) (Clock) in
        let%lwt result = Usecase.execute {name = "foo"; node_id = "bar"} in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "not found filer"
          (Error `Not_found_filer)
          result ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "issue error when node not found" `Quick (fun _ () ->
        let file_tree =
          D.File_tree.make ~location:Path.(of_string "/bar") ~nodes:[TF.Node.fixture dir_stat]
        in
        let filer =
          D.Filer.make ~id:"foo" ~file_tree
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name ()
        in
        let module FR = (val TF.Memory_repository.filer_repository ~initial:[filer] ()) in
        let module Svc = struct
          let scan _ = assert false
        end in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Enter_directory.Make (FR) (Svc) (Clock) in
        let%lwt result = Usecase.execute {name = "foo"; node_id = "not found"} in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "not found node"
          (Error `Not_found_node)
          result ;
        Lwt.return_unit ) ]
