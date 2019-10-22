open Sxfiler_core
module D = Sxfiler_domain
module I = Sxfiler_server_infra

module Dummy_ns () = struct
  type location = Path.t

  let called = ref 0
  let send ~typ:_ _ = incr called ; Lwt.return_unit
end

module Message_factory = I.Message_notification_factory.Make (struct
  type id = Uuidm.t

  let id = Uuidm.v4_gen (Random.State.make [||]) ()
  let generate () = id
end)

module Progress_factory = I.Progress_notification_factory.Make (struct
  type id = Uuidm.t

  let id = Uuidm.v4_gen (Random.State.make [||]) ()
  let generate () = id
end)

let test_set =
  [ Alcotest_lwt.test_case "trash a file" `Quick (fun _ () ->
        let open Fun in
        let module M = I.Item_trash_service.Make (Dummy_ns ()) (Message_factory) (Progress_factory)
        in
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
               Lwt.return_unit))
  ; Alcotest_lwt.test_case "call notification per item" `Quick (fun _ () ->
        let open Fun in
        let module NS = Dummy_ns () in
        let module M = I.Item_trash_service.Make (NS) (Message_factory) (Progress_factory) in
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
               Alcotest.(check int) "count" 2 !NS.called ;
               Lwt.return_unit)) ]
