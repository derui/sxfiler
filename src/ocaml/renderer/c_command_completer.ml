(** {!C_command_completer} defines container component to input and completer for command. *)

module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module Store = Sxfiler_renderer_store
module U = Sxfiler_renderer_usecase
module S = Sxfiler_renderer_service
module SI = Sxfiler_renderer_service_impl
module T = Sxfiler_rpc.Types
module Command = Sxfiler_renderer_command

let complete_command (module L: Locator.S) command =
  let module Ctx = (val L.context) in
  let module Service = SI.Completion.Make((val L.client)) in
  let module B = (val C.Usecase.make_instance (module U.Read_completion.Make(Service))
      ~param:command) in
  Ctx.(Context.execute this (module B)) |> Lwt.ignore_result

let setup_command_source props _ =
  let module DL = Command.Dynamic_registry in
  let module L = (val props##.locator: Locator.S) in
  let commands = DL.names L.dynamic_command_registry
                 |> List.map (fun action_name ->
                     {T.Completion.Item.id = action_name; value = action_name})
  in
  let module Ctx = (val L.context) in
  let param = (commands, "command_completer") in
  let module Service = SI.Completion.Make((val L.client)) in
  let instance = C.Usecase.make_instance
      (module U.Setup_completion.Make(Service)) ~param in
  let open Lwt in
  ignore_result @@ (let%lwt () = Lwt_js.yield () in
                    Ctx.(Context.execute this instance))

let clean_up_completer props _ =
  let module L = (val props##.locator : Locator.S) in
  let module Ctx = (val L.context) in
  let instance = C.Usecase.make_instance (module U.Cleanup_completion) ~param:() in
  let open Lwt in
  ignore_result @@ (Lwt_js.yield () >>= fun () ->
                    Ctx.(Context.execute this instance))

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method locator: (module Locator.S) Js.readonly_prop
             end
           end)
    ~render:(fun props ->
        let module L = (val props##.locator : Locator.S) in
        let state = Store.App.Store.get L.store in
        let ws = Store.Workspace.Store.get @@ Store.App.State.workspace state in
        let focused = Store.Workspace.State.match_current_mode ws ~mode:C.Types.Mode.Complete in

        [%c P_completer_wrapper.t
            ~props:(object%js
              val completerId = "command_completer"
              val completion = Store.(Completion.Store.get @@ App.State.completion state)
            end)
            [[%c P_command_selector.t ~key:"completer" ~props:(object%js
                val focused = focused
                val onFocus = (setup_command_source props)
                val onBlur = (clean_up_completer props)
                val onChangeCommand = (complete_command (module L))
              end)
            ]
            ]
        ]
      )
