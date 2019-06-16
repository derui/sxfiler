open Sxfiler_core
module D = Sxfiler_domain
module I = Sxfiler_server_infra

module Dummy_ns = struct
  type location = Path.t

  let send _ = Lwt.return_unit
end

module Dummy_factory : D.Notification.Factory = struct
  let id = Uuidm.v4_gen (Random.State.make [||]) ()
  let create ~level ~body = D.Notification.make ~id ~level ~body
end

let test_set =
  [ Alcotest_lwt.test_case "replicate a node to destination" `Quick (fun _ () ->
        let module M = I.Node_replication_service.Make (Dummy_ns) (Dummy_factory) in
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "node" "sample" in
        let stat = Test_fixtures.File_stat.fixture () in
        let node = Test_fixtures.Node.fixture ~full_path:Path.(of_string temp_file) stat in
        let finalizer () = File.remove temp_dir ; File.remove dest ; Lwt.return_unit in
        finalizer
        |> Lwt.finalize (fun () ->
            let _to = D.File_tree.make ~location:Path.(of_string dest) ~nodes:[] in
            let%lwt () = M.replicate ~node ~_to () in
            let dest_file = Filename.basename temp_file |> Filename.concat dest in
            Alcotest.(check bool) "destination" true (Sys.file_exists dest_file) ;
            Alcotest.(check bool) "source" true (Sys.file_exists temp_file) ;
            Lwt.return_unit ) )
  ; Alcotest_lwt.test_case "replicate nodes to destination with correction" `Quick (fun _ () ->
        let module M = I.Node_replication_service.Make (Dummy_ns) (Dummy_factory) in
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "node" "sample" in
        let stat = Test_fixtures.File_stat.fixture () in
        let node = Test_fixtures.Node.fixture ~full_path:Path.(of_string temp_file) stat in
        let finalizer () = File.remove temp_dir ; File.remove dest ; Lwt.return_unit in
        finalizer
        |> Lwt.finalize (fun () ->
            let _to = D.File_tree.make ~location:Path.(of_string dest) ~nodes:[] in
            let%lwt () = M.replicate ~node ~new_name:"renamed" ~_to () in
            let dest_file = Filename.concat dest "renamed" in
            Alcotest.(check bool) "destination" true (Sys.file_exists dest_file) ;
            Alcotest.(check bool) "source" true (Sys.file_exists temp_file) ;
            Lwt.return_unit ) ) ]
