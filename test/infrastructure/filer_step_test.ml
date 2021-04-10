open Sxfiler_core
module D = Sxfiler_domain
module I = Sxfiler_infrastructure
module F = Sxfiler_workflow

let get_instance () =
  let module S = I.Statable.Make (struct
    type t = D.Filer.t option

    let empty () = None
  end) in
  (module I.Filer_step.Instance (S) : F.Common_step.Filer.Instance)

let copy_item_test =
  [
    Alcotest_lwt.test_case "copy a item to destination" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let finalizer () =
          File.remove temp_dir;
          File.remove dest;
          Lwt.return_unit
        in
        let module Instance = (val get_instance ()) in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt ret =
                 Instance.copy_item
                   {
                     source = Path.of_string temp_file |> Result.get_ok;
                     dest = Path.of_list [ dest; Filename.basename temp_file ] |> Result.get_ok;
                     overwrite = false;
                   }
               in
               let dest_file = Filename.basename temp_file |> Filename.concat dest in
               Alcotest.(check @@ result unit @@ of_pp F.Common_step.Filer.pp_operation_error) "result" (Ok ()) ret;
               Alcotest.(check bool) "destination" true (Sys.file_exists dest_file);
               Alcotest.(check bool) "source" true (Sys.file_exists temp_file);
               Lwt.return_unit));
    Alcotest_lwt.test_case "raise error when target is not found" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let finalizer () =
          File.remove temp_dir;
          File.remove dest;
          Lwt.return_unit
        in
        let module Instance = (val get_instance ()) in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt ret =
                 Instance.copy_item
                   {
                     source = Path.of_string temp_file |> Result.get_ok;
                     dest = Path.of_list [ dest ^ "_ignore"; Filename.basename temp_file ] |> Result.get_ok;
                     overwrite = false;
                   }
               in
               let expected = Path.of_list [ dest ^ "_ignore"; Filename.basename temp_file ] |> Result.get_ok in
               Alcotest.(check @@ result unit @@ of_pp F.Common_step.Filer.pp_operation_error)
                 "ret" (Error (F.Common_step.Filer.Not_exists expected)) ret;
               Lwt.return_unit));
  ]

and move_item_test =
  [
    Alcotest_lwt.test_case "transport a item to destination" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let finalizer () =
          File.remove temp_dir;
          File.remove dest;
          Lwt.return_unit
        in
        let module Instance = (val get_instance ()) in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt _ =
                 Instance.move_item
                   {
                     source = Path.of_string temp_file |> Result.get_ok;
                     dest = Path.of_list [ dest; Filename.basename temp_file ] |> Result.get_ok;
                     overwrite = false;
                   }
               in
               let dest_file = Filename.basename temp_file |> Filename.concat dest in
               Alcotest.(check bool) "destination" true (Sys.file_exists dest_file);
               Alcotest.(check bool) "source" false (Sys.file_exists temp_file);
               Lwt.return_unit));
    Alcotest_lwt.test_case "transport items to destination another name" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let dest' = Path.(of_list [ dest; "renamed" ]) |> Result.get_ok in
        let finalizer () =
          File.remove temp_dir;
          File.remove dest;
          Lwt.return_unit
        in
        let module Instance = (val get_instance ()) in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt ret =
                 Instance.move_item
                   { source = Path.of_string temp_file |> Result.get_ok; dest = dest'; overwrite = false }
               in
               let dest_file = Filename.concat dest "renamed" in
               Alcotest.(check @@ result unit @@ of_pp F.Common_step.Filer.pp_operation_error) "ret" (Ok ()) ret;
               Alcotest.(check bool) "destination" true (Sys.file_exists dest_file);
               Alcotest.(check bool) "source" false (Sys.file_exists temp_file);
               Lwt.return_unit));
  ]

and delete_item_test =
  [
    Alcotest_lwt.test_case "delete an item" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let finalizer () =
          File.remove temp_dir;
          Lwt.return_unit
        in
        let module Instance = (val get_instance ()) in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt items = I.File_list_step.Instance.scan_location (Path.of_string temp_dir |> Result.get_ok) in
               let%lwt ret =
                 match items with
                 | Ok [ item ]    -> Instance.delete_item item
                 | Ok _ | Error _ -> Alcotest.fail "illegal path"
               in

               Alcotest.(check @@ result unit @@ of_pp Fmt.nop) "result" (Ok ()) ret;
               Alcotest.(check bool) "source" false (Sys.file_exists temp_file);
               Lwt.return_unit));
  ]

let test_set = List.concat [ copy_item_test; move_item_test; delete_item_test ]
