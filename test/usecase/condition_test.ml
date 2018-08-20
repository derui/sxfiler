module D = Sxfiler_domain
module U = Sxfiler_usecase.Condition

let testcases = [
  Alcotest_lwt.test_case "can enable context and update singleton condition" `Quick (fun _ () ->
      let module R = (struct
        let cond = ref D.Condition.empty
        let resolve () = Lwt.return !cond
        let store t = Lwt.return (cond := t)
      end)
      in
      let module Case = U.Enable(R) in
      let%lwt ret = Case.execute {context = "foo"} in
      Alcotest.(check @@ result unit (of_pp Fmt.nop)) "result" (Ok ()) ret;

      let expected = D.Condition.empty |> D.Condition.enable ~context:"foo" in
      Lwt.return @@ Alcotest.(check bool) "same" true D.Condition.(equal !R.cond expected)
    );
  Alcotest_lwt.test_case "can disable context and update singleton condition" `Quick (fun _ () ->
      let module R = (struct
        let cond = ref D.Condition.(empty |> enable ~context:"foo")
        let resolve () = Lwt.return !cond
        let store t = Lwt.return (cond := t)
      end)
      in
      let module Case = U.Disable(R) in
      let%lwt ret = Case.execute {context = "foo"} in
      Alcotest.(check @@ result unit (of_pp Fmt.nop)) "result" (Ok ()) ret;

      let expected = D.Condition.empty in
      Lwt.return @@ Alcotest.(check bool) "same" true D.Condition.(equal !R.cond expected)
    );
]
