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
  [ Alcotest_lwt.test_case "transport do nothing if nodes is empty" `Quick (fun _ () ->
        let module R = I.Node_transporter_service in
        let _to = Path.of_string "foo" in
        let module M = R.Make (Dummy_ns) (Dummy_factory) in
        let%lwt () = M.transport ~nodes:[] ~corrections:[] ~_to in
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "transport nodes to destination" `Quick (fun _ () ->
        let open Fun in
        let module M = I.Node_transporter_service.Make (Dummy_ns) (Dummy_factory) in
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "node" "sample" in
        let get_stat = Unix.lstat %> I.Conv.stat_to_file_stat in
        let node =
          D.Node.make ~id:"foo" ~link_path:None ~stat:(get_stat temp_file)
            ~full_path:Path.(of_string temp_file)
        in
        let finalizer () = File.remove temp_dir ; File.remove dest ; Lwt.return_unit in
        finalizer
        |> Lwt.finalize (fun () ->
            let _to = Path.of_string dest in
            let%lwt () = M.transport ~nodes:[node] ~corrections:[] ~_to in
            let dest_file = Filename.basename temp_file |> Filename.concat dest in
            Alcotest.(check bool) "destination" true (Sys.file_exists dest_file) ;
            Alcotest.(check bool) "source" false (Sys.file_exists temp_file) ;
            Lwt.return_unit ) )
  ; Alcotest_lwt.test_case "transport nodes to destination with correction" `Quick (fun _ () ->
        let open Fun in
        let module M = I.Node_transporter_service.Make (Dummy_ns) (Dummy_factory) in
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "node" "sample" in
        let get_stat = Unix.lstat %> I.Conv.stat_to_file_stat in
        let node =
          D.Node.make ~id:"foo" ~link_path:None ~stat:(get_stat temp_file)
            ~full_path:Path.(of_string temp_file)
        in
        let finalizer () = File.remove temp_dir ; File.remove dest ; Lwt.return_unit in
        finalizer
        |> Lwt.finalize (fun () ->
            let _to = Path.of_string dest in
            let%lwt () =
              M.transport ~nodes:[node]
                ~corrections:[D.Types.Correction.name "foo" "renamed"]
                ~_to
            in
            let dest_file = Filename.concat dest "renamed" in
            Alcotest.(check bool) "destination" true (Sys.file_exists dest_file) ;
            Alcotest.(check bool) "source" false (Sys.file_exists temp_file) ;
            Lwt.return_unit ) ) ]
