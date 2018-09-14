(** Keymap module provides function to refresh keymap in background. *)

module C = Sxfiler_renderer_core
module T = Sxfiler_rpc.Types
module S = Sxfiler_renderer_store
module Svc = Sxfiler_renderer_service

type t = {action : (module Types.Action)}

let instance : t option ref = ref None

(* the singleton instance for planner *)
let initialize store =
  instance :=
    Some
      { action =
          ( module struct
            let state () = store
          end ) }

(* [start ()] runs planner main loop asynchronous. This function returns tuple [(sender, stopper)].
   User can send message via [sender] and stop loop via [stopper]. *)
let start (module D : C.Dispatcher.Instance) (module Svc : Svc.Service_registry.S) =
  let open Sxfiler_core.Option in
  !instance
  >|= fun instance ->
  let accepter : C.Message.t Lwt_mvar.t = Lwt_mvar.create_empty () in
  let message_accepter message = Lwt.async (fun () -> Lwt_mvar.put accepter message) in
  let rec main_loop t =
    let reset_modes () =
      let module K = (val Svc.keymap ()) in
      let all_modes = C.Types.Mode.(List.map to_context all_modes) in
      Lwt_list.map_p (fun context -> K.delete_context {context}) all_modes
    in
    match%lwt Lwt_mvar.take accepter with
    | C.Message.Focus_mode mode ->
      let%lwt _ = reset_modes () in
      let module K = (val Svc.keymap ()) in
      let context = C.Types.Mode.to_context mode in
      let%lwt keymap = K.add_context {context} in
      D.(Dispatcher.dispatch this C.Message.(Update_keymap keymap)) ;
      main_loop t
    | C.Message.Blur_mode ->
      let open Sxfiler_core.Fun in
      let%lwt _ = reset_modes () in
      let to_context = S.Workspace.State.current_mode %> C.Types.Mode.to_context in
      let module A = (val t.action) in
      let module K = (val Svc.keymap ()) in
      let context =
        (A.state %> S.App.State.workspace %> S.Workspace.Store.get %> to_context) ()
      in
      let%lwt keymap = K.add_context {context} in
      D.(Dispatcher.dispatch this C.Message.(Update_keymap keymap)) ;
      main_loop t
    | _ -> main_loop t
  in
  let loop = main_loop instance in
  Lwt.async (fun () -> loop) ;
  (message_accepter, fun () -> Lwt.cancel loop)
