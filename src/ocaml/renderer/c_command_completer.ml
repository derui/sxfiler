(** {!C_command_completer} defines container component to input and completer for command. *)

module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module Be = Sxfiler_renderer_behavior

module Component = R.Component.Make_stateless(struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
    end
  end)

let complete_command (module L: Locator.Main) command =
  let module Ctx = (val L.context) in
  let module B = (val C.Behavior.make_instance (module Be.Read_completion)
                     ~config:(module L)
                     ~param:command) in
  Ctx.(Context.execute this (module B)) |> Lwt.ignore_result

let t = Component.make (fun props ->
    let module L = (val props##.locator : Locator.Main) in
    let state = S.App.Store.get L.store in

    let on_focus props _ =
      let module DL = C.Locator_intf.Dynamic_registry in
      let module L = (val props##.locator: Locator.Main) in
      let commands = DL.names L.dynamic_command_registry
                     |> List.map (fun action_name -> {T.Completion.Item.id = action_name;
                                                      value = action_name})
      in
      let module Ctx = (val L.context) in
      let param = (T.Completion.Source_class.Simple, commands, "command_completer") in
      let module I = (val C.Behavior.make_instance
                         (module Be.Setup_completion) ~config:(module L) ~param) in
      Ctx.(Context.execute this (module I)) |> Lwt.ignore_result

    in

    [%c C_completer_wrapper.t
        ~completerId:"command"
        ~completion:(S.Completion.Store.get @@ S.App.State.completion state)
        ~locator:props##.locator
        ~onCompleted:(fun _ -> failwith "not implemented yet")
        [[%c P_command_selector.t ~key:"completer"
            ~onFocus:(on_focus props)
            ~onChangeCommand:(complete_command (module L))]
        ]
    ]
  )
