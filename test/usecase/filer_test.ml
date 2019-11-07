open Sxfiler_core
module F = Test_fixtures
module D = Sxfiler_domain
module U = Sxfiler_usecase

module Factory = D.Filer.Factory.Make (struct
  type id = D.Filer.id

  let state = Random.get_state ()
  let generate () = Uuidm.v4_gen state ()
end)

let dir_stat = Test_fixtures.File_stat.fixture ~directory:true ()

let test_set =
  let module TF = Test_fixtures in
  [
    Alcotest_lwt.test_case "renewal filer with entered directory" `Quick (fun _ () ->
        let file_list =
          D.File_list.make
            ~location:Path.(of_string "/bar")
            ~items:[ TF.File_item.fixture dir_stat ] ()
        in
        let filer = Factory.create ~name:"foo" ~file_list ~sort_order:D.Types.Sort_type.Name in
        let module FR =
        (val Test_fixtures.Memory_repository.filer_repository ~initial:[ filer ] ())
        in
        let new_items = [ TF.File_item.fixture ~full_path:Path.(of_string "new") dir_stat ] in
        let module Svc =
        (val TF.Service.location_scanner_service Path.(of_string "/bar") new_items)
        in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Enter_directory.Make (FR) (Svc) (Clock) in
        let%lwt result =
          Usecase.execute { name = "foo"; item_id = List.hd file_list.items |> D.File_item.id }
        in
        let%lwt data = Lwt.(FR.resolve_by_name "foo" >|= Option.get_exn) in
        Alcotest.(check @@ result (of_pp D.Filer.pp) (of_pp Fmt.nop)) "renew filer" (Ok data) result;
        Lwt.return_unit);
    Alcotest_lwt.test_case "renewal filer with new location" `Quick (fun _ () ->
        let file_list =
          D.File_list.make
            ~location:Path.(of_string "/bar")
            ~items:[ TF.File_item.fixture dir_stat ] ()
        in
        let filer = Factory.create ~name:"foo" ~file_list ~sort_order:D.Types.Sort_type.Name in
        let module FR =
        (val Test_fixtures.Memory_repository.filer_repository ~initial:[ filer ] ())
        in
        let new_items = [ TF.File_item.fixture ~full_path:Path.(of_string "new") dir_stat ] in
        let module Svc =
        (val TF.Service.location_scanner_service Path.(of_string "/foo") new_items)
        in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Jump_location.Make (FR) (Svc) (Clock) in
        let%lwt result = Usecase.execute { name = "foo"; location = Path.of_string "/foo" } in
        let%lwt data = Lwt.(FR.resolve_by_name "foo" >|= Option.get_exn) in
        Alcotest.(check @@ result (of_pp D.Filer.pp) (of_pp Fmt.nop)) "renew filer" (Ok data) result;
        Lwt.return_unit);
    Alcotest_lwt.test_case "error when filer not found" `Quick (fun _ () ->
        let module FR = (val TF.Memory_repository.filer_repository ()) in
        let module Svc = struct
          let scan _ = assert false
        end in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Enter_directory.Make (FR) (Svc) (Clock) in
        let%lwt result = Usecase.execute { name = "foo"; item_id = "bar" } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "not found filer"
          (Error `Not_found_filer)
          result;
        Lwt.return_unit);
    Alcotest_lwt.test_case "issue error when item not found" `Quick (fun _ () ->
        let file_list =
          D.File_list.make
            ~location:Path.(of_string "/bar")
            ~items:[ TF.File_item.fixture dir_stat ] ()
        in
        let filer = Factory.create ~name:"foo" ~file_list ~sort_order:D.Types.Sort_type.Name in
        let module FR = (val TF.Memory_repository.filer_repository ~initial:[ filer ] ()) in
        let module Svc = struct
          let scan _ = assert false
        end in
        let module Clock = struct
          let unixtime () = Int64.min_int
        end in
        let module Usecase = U.Filer.Enter_directory.Make (FR) (Svc) (Clock) in
        let%lwt result = Usecase.execute { name = "foo"; item_id = "not found" } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "not found item"
          (Error `Not_found_item)
          result;
        Lwt.return_unit);
  ]
