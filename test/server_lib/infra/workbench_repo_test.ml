open Sxfiler_core
module D = Sxfiler_domain
module C = Sxfiler_server_core
module I = Sxfiler_server_infra

let filer =
  D.Filer.make ~id:"foo" ~location:(Path.of_string "/var") ~nodes:[]
    ~sort_order:D.Types.Sort_type.Date ~history:(D.Location_history.make ())

module Factory = struct
  let id = Uuidm.v4_gen (Random.get_state ()) ()
  let make env = {D.Workbench.env; id; corrections = []}
end

let test_set =
  [ Alcotest_lwt.test_case "can store and resolve the workbench with id" `Quick (fun _ () ->
        let module State = C.Statable.Make (struct
            type t = C.Workbench_state.t

            let empty () = C.Workbench_state.empty
          end) in
        let module R = I.Workbench_repo.Make (State) in
        let data = Factory.make {source = filer; dest = filer; nodes = []} in
        let%lwt () = R.store data in
        let%lwt actual = R.resolve Factory.id in
        Alcotest.(check @@ option @@ of_pp Fmt.nop) "stored" (Some data) actual ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "return None if workbench did not stored" `Quick (fun _ () ->
        let module State = C.Statable.Make (struct
            type t = C.Workbench_state.t

            let empty () = C.Workbench_state.empty
          end) in
        let module R = I.Workbench_repo.Make (State) in
        let%lwt actual = R.resolve Factory.id in
        Alcotest.(check @@ option @@ of_pp Fmt.nop) "stored" None actual ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "can remove the workbench stored already" `Quick (fun _ () ->
        let module State = C.Statable.Make (struct
            type t = C.Workbench_state.t

            let empty () = C.Workbench_state.empty
          end) in
        let module R = I.Workbench_repo.Make (State) in
        let data = Factory.make {source = filer; dest = filer; nodes = []} in
        let%lwt () = R.store data in
        let%lwt actual = R.resolve Factory.id in
        let%lwt () = R.remove data in
        let%lwt actual' = R.resolve Factory.id in
        Alcotest.(check @@ option @@ of_pp Fmt.nop) "stored" (Some data) actual ;
        Alcotest.(check @@ option @@ of_pp Fmt.nop) "removed" None actual' ;
        Lwt.return_unit ) ]
