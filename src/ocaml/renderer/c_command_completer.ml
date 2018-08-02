(** {!C_command_completer} defines container component to input and completer for command. *)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module Be = Sxfiler_renderer_behavior

module Component = R.Component.Make_stateful(struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
    end
  end)
    (struct
      type t = unit
    end)

let t =
  let render = (fun this ->
    let module L = (val this##.props##.locator : Locator.Main) in
    let state = S.App.Store.get L.store in

    [%c C_completer_wrapper.t
        ~completerId:"command"
        ~completion:(S.Completion.Store.get @@ S.App.State.completion state)
        ~locator:this##.props##.locator
          [[%c P_command_selector.t ~key:"completer"
              ~onChangeCommand:(fun _ -> failwith "not implemented yet")]
          ]
    ]
  ) in
  let spec = R.component_spec render
      ~component_did_mount:(fun this ->
          let module L = (val this##.props##.locator: Locator.Main) in
          let commands = C.Command.Registry.to_action_list L.command_registry
                       |> List.map (C.Callable_action.to_string)
                       |> List.map (fun action_name -> {T.Completion.Item.id = action_name;
                                                       value = action_name})
          in
          let module Ctx = (val L.context) in
          let param = (T.Completion.Source_class.Simple, commands, "command_completer") in
          let module I = (val C.Behavior.make_instance
                             (module Be.Setup_completion) ~config:(module L) ~param) in
          Ctx.(Context.execute this (module I)) |> Lwt.ignore_result

        )
  in
  Component.make spec
