open Sxfiler_core
module D = Sxfiler_domain
module I = Sxfiler_server_infra

module Dummy_ns = struct
  type location = Path.t

  let send ~typ:_ _ = Lwt.return_unit
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
  let suggest item =
    let suggestion =
      D.Task_interaction.Suggestion.
        { task_id = Uuidm.v4_gen (Random.State.make [||]) ()
        ; item_name = Path.basename item.D.File_item.full_path
        ; suggestions = [] }
    in
    (suggestion, Lwt.return (D.Task_interaction.Reply.Rename "renamed"))
  in
  let module M = I.Item_transporter_service.Make (Dummy_ns) (Message_factory) (Progress_factory) in
  [ Alcotest_lwt.test_case "transport a item to destination" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let stat = Test_fixtures.File_stat.fixture () in
        let item = Test_fixtures.File_item.fixture ~full_path:Path.(of_string temp_file) stat in
        let finalizer () = File.remove temp_dir ; File.remove dest ; Lwt.return_unit in
        finalizer
        |> Lwt.finalize (fun () ->
            let _to = D.File_list.make ~location:Path.(of_string dest) ~items:[] () in
            let%lwt () = M.transport ~suggest ~items:[item] ~_to in
            let dest_file = Filename.basename temp_file |> Filename.concat dest in
            Alcotest.(check bool) "destination" true (Sys.file_exists dest_file) ;
            Alcotest.(check bool) "source" false (Sys.file_exists temp_file) ;
            Lwt.return_unit))
  ; Alcotest_lwt.test_case "transport items to destination with suggestion" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let stat = Test_fixtures.File_stat.fixture () in
        let item = Test_fixtures.File_item.fixture ~full_path:Path.(of_string temp_file) stat in
        let item_to =
          Test_fixtures.File_item.fixture
            ~full_path:Path.(of_list [dest; Filename.basename temp_file])
            stat
        in
        let finalizer () = File.remove temp_dir ; File.remove dest ; Lwt.return_unit in
        finalizer
        |> Lwt.finalize (fun () ->
            let _to = D.File_list.make ~location:Path.(of_string dest) ~items:[item_to] () in
            let%lwt () = M.transport ~suggest ~items:[item] ~_to in
            let dest_file = Filename.concat dest "renamed" in
            Alcotest.(check bool) "destination" true (Sys.file_exists dest_file) ;
            Alcotest.(check bool) "source" false (Sys.file_exists temp_file) ;
            Lwt.return_unit)) ]
