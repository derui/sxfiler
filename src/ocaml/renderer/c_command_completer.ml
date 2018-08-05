(** {!C_command_completer} defines container component to input and completer for command. *)

module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module Store = Sxfiler_renderer_store
module U = Sxfiler_renderer_usecase
module S = Sxfiler_renderer_service
module SI = Sxfiler_renderer_service_impl

module Component = R.Component.Make_stateless(struct
    class type t = object
      method locator: (module Locator.S) Js.readonly_prop
    end
  end)

let complete_command (module L: Locator.S) command =
  let module Ctx = (val L.context) in
  let module Service = SI.Completion.Make((val L.client)) in
  let module B = (val C.Usecase.make_instance (module U.Read_completion.Make(Service))
                     ~param:command) in
  Ctx.(Context.execute this (module B)) |> Lwt.ignore_result

let t = Component.make (fun props ->
    let module L = (val props##.locator : Locator.S) in
    let state = Store.App.Store.get L.store in

    let on_focus props _ =
      let module DL = C.Command.Dynamic_registry in
      let module L = (val props##.locator: Locator.S) in
      let module T = Sxfiler_completion.Domain in
      let commands = DL.names L.dynamic_command_registry
                     |> List.map (fun action_name -> {T.Item.id = action_name;
                                                      value = action_name})
      in
      let module Ctx = (val L.context) in
      let param = (commands, "command_completer") in
      let module Service = SI.Completion.Make((val L.client)) in
      let module I = (val C.Usecase.make_instance
                         (module U.Setup_completion.Make(Service)) ~param) in
      Ctx.(Context.execute this (module I)) |> Lwt.ignore_result

    in

    [%c C_completer_wrapper.t
        ~completerId:"command"
        ~completion:(Store.(Completion.Store.get @@ App.State.completion state))
        ~locator:props##.locator
        ~onCompleted:(fun _ -> failwith "not implemented yet")
        [[%c P_command_selector.t ~key:"completer"
            ~onFocus:(on_focus props)
            ~onChangeCommand:(complete_command (module L))]
        ]
    ]
  )
