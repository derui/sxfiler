open Sxfiler_core
module D = Sxfiler_domain
module I = Sxfiler_server_infra

let test_set =
  [ Alcotest_lwt.test_case "trash a file" `Quick (fun _ () ->
        let open Fun in
        let module M = I.Item_trash_service in
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let get_stat = Unix.lstat %> I.Conv.stat_to_file_stat in
        let item =
          D.File_item.make ~id:"foo" ~link_path:None ~stat:(get_stat temp_file)
            ~full_path:Path.(of_string temp_file)
        in
        let finalizer () = File.remove temp_dir ; Lwt.return_unit in
        finalizer
        |> Lwt.finalize (fun () ->
            let _to = Path.of_string dest in
            let%lwt () = M.trash [item] in
            Alcotest.(check bool) "source" false (Sys.file_exists temp_file) ;
            Lwt.return_unit ) ) ]
