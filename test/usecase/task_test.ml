module D = Sxfiler_domain
module U = Sxfiler_usecase

let test_set =
  [
    Alcotest_lwt.test_case "cancel the task" `Quick (fun _ () ->
        let task_id = Uuidm.v4_gen (Random.get_state ()) () in
        let task =
          D.Task.make ~id:task_id
            ~executor:
              ( module struct
                let apply_interaction = `No_interaction
                let execute _ = failwith "mocked"
              end : D.Task.Executor )
        in
        let module KR = (val Test_fixtures.Memory_repository.task_repository [ task ]) in
        let module Usecase = U.Task.Cancel.Make (KR) in
        let%lwt result = Usecase.execute task_id in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "key map" (Ok ()) result;
        Lwt.return_unit);
    Alcotest_lwt.test_case "get error when task not found" `Quick (fun _ () ->
        let task_id = Uuidm.v4_gen (Random.get_state ()) () in
        let module KR = (val Test_fixtures.Memory_repository.task_repository []) in
        let module Usecase = U.Task.Cancel.Make (KR) in
        let%lwt result = Usecase.execute task_id in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "key map"
          (Error `Not_found)
          result;
        Lwt.return_unit);
  ]
