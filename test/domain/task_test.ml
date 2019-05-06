module D = Sxfiler_domain
module TF = Test_fixtures

module Clock = struct
  let unixtime () = Int64.min_int
end

module Id_gen = struct
  type id = Uuidm.t

  let seed = Random.State.make [|0|]
  let id = Uuidm.v4_gen seed ()
  let generate () = id
end

let test_set =
  [ ( Alcotest_lwt.test_case "run executor with self id" `Quick
      @@ fun _ () ->
      let module F = D.Task.Factory.Make (Id_gen) in
      let task =
        F.create
          ~executor:
            ( module struct
              let apply_interaction = `No_interaction

              let execute {D.Task.task_id} =
                Alcotest.(check @@ of_pp Uuidm.pp) "subset" Id_gen.id task_id ;
                Lwt.return_unit
            end )
      in
      D.Task.execute task )
  ; ( Alcotest_lwt.test_case "apply user interaction to the task if it can apply user interaction"
        `Quick
      @@ fun _ () ->
      let open Sxfiler_core in
      let module F = D.Task.Factory.Make (Id_gen) in
      let s, f = Spy.wrap (Fun.const Lwt.return_unit) in
      let expected = D.Task.Interaction.String "foo" in
      let open Lwt in
      let task =
        F.create
          ~executor:
            ( module struct
              let apply_interaction = `Apply f
              let execute _ = Lwt.return_unit
            end )
      in
      D.Task.apply_interaction ~interaction:expected task
      >|= fun () ->
      Alcotest.(check @@ list @@ of_pp D.Task.Interaction.pp)
        "interaction" [expected] (Spy.Wrap.called_args s) ) ]
