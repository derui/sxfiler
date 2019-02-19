module D = Sxfiler_domain
module U = Sxfiler_usecase

let test_set =
  [ Alcotest_lwt.test_case "return current configuration" `Quick (fun _ () ->
        let configuration = D.Configuration.default in
        let module CR = struct
          let data = ref configuration
          let resolve () = Lwt.return configuration

          let store v =
            data := v ;
            Lwt.return_unit
        end in
        let module Usecase = U.Configuration.Get.Make (CR) in
        let%lwt result = Usecase.execute () in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "renew filer" (Ok !CR.data) result ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "return current configuration" `Quick (fun _ () ->
        let configuration = D.Configuration.default in
        let module CR = struct
          let data = ref None
          let resolve () = assert false

          let store v =
            data := Some v ;
            Lwt.return_unit
        end in
        let module Usecase = U.Configuration.Store.Make (CR) in
        let%lwt _ = Usecase.execute configuration in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "stored" (Ok !CR.data) (Ok (Some configuration)) ;
        Lwt.return_unit ) ]
