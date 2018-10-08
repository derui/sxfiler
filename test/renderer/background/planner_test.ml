(* Test cases for core component for planner. *)

open Sxfiler_core
open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module P = Sxfiler_renderer_background.Planner
module S = Sxfiler_renderer_store

let state =
  let config = S.Config.(State.make () |> Store.make)
  and file_list = S.File_list.(State.make () |> Store.make)
  and keymap = S.Keymap.(State.make () |> Store.make)
  and completion = S.Completion.(State.make () |> Store.make)
  and command = S.Command.(State.make () |> Store.make)
  and workspace = S.Workspace.(State.make () |> Store.make)
  and notification = S.Notification.(State.make () |> Store.make) in
  S.App.State.make ~config ~file_list ~keymap ~completion ~command ~workspace ~notification

let () =
  "Planner"
  >::: [ ( "should be able to run normal process"
           >:- fun () ->
             let () = P.initialize state in
             let accepter, stopper = P.start () |> Option.get_exn in
             let plan_waiter, plan_wakener = Lwt.task ()
             and execute_waiter, execute_wakener = Lwt.task () in
             let executor =
               { P.execute = (fun ~action:_ -> Lwt.wakeup execute_wakener () |> Lwt.return)
               ; plan = (fun _ ~action:_ -> Lwt.wakeup plan_wakener () |> Lwt.return) }
             in
             P.reserve_executor executor |> ignore ;
             Lwt.async (fun () ->
                 let%lwt () = Lwt_js.yield () in
                 accepter C.Message.(Command Approve) |> Lwt.return ) ;
             let%lwt () = Lwt.join [plan_waiter; execute_waiter] in
             stopper () ;
             assert_ok true |> Lwt.return )
       ; ( "should not execute if plan rejected"
           >:- fun () ->
             let () = P.initialize state in
             let accepter, stopper = P.start () |> Option.get_exn in
             let plan_waiter, plan_wakener = Lwt.task ()
             and execute_waiter, execute_wakener = Lwt.task () in
             let executor =
               { P.execute = (fun ~action:_ -> Lwt.wakeup execute_wakener () |> Lwt.return)
               ; plan = (fun _ ~action:_ -> Lwt.wakeup plan_wakener () |> Lwt.return) }
             in
             P.reserve_executor executor |> ignore ;
             Lwt.async (fun () ->
                 let%lwt () = Lwt_js.yield () in
                 accepter C.Message.(Command Command.Reject) ;
                 accepter C.Message.(Command Command.Approve) |> Lwt.return ) ;
             let%lwt () = plan_waiter in
             stopper () ;
             match Lwt.state execute_waiter with
             | Lwt.Sleep -> assert_ok true |> Lwt.return
             | _ -> assert_fail "do not stop" |> Lwt.return )
       ; ( "should be able to handle conflict repeated"
           >:- fun () ->
             let () = P.initialize state in
             let accepter, stopper = P.start () |> Option.get_exn in
             let plan_waiter, plan_wakener = Lwt.task ()
             and execute_waiter, execute_wakener = Lwt.task () in
             let data = ref 0 in
             let executor =
               { P.execute = (fun ~action:_ -> Lwt.wakeup execute_wakener () |> Lwt.return)
               ; plan =
                   (fun _ ~action:_ ->
                      incr data ;
                      if !data >= 3 then Lwt.wakeup plan_wakener () |> Lwt.return else Lwt.return_unit
                   ) }
             in
             P.reserve_executor executor |> ignore ;
             Lwt.async (fun () ->
                 let%lwt () = Lwt_js.yield () in
                 accepter C.Message.(Command Command.(Remains_conflict)) ;
                 accepter C.Message.(Command Command.(Remains_conflict)) ;
                 accepter C.Message.(Command Command.Approve) |> Lwt.return ) ;
             let%lwt () = Lwt.join [plan_waiter; execute_waiter] in
             stopper () ;
             assert_ok true |> Lwt.return ) ]
