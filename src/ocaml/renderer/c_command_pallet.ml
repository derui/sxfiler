(** {!C_command_pallet} defines container component to be used to input and execute commands. *)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module B = Sxfiler_renderer_behavior

module Component = R.Component.Make_stateful(struct
    class type t = object
      method context: (module Context.Instance) Js.readonly_prop
    end
  end)(struct
    class type t = object
      method command: C.Command.Class.t option Js.readonly_prop
    end
  end)

let show_command_selector context =
  let module I = (val context : Context.Instance) in
  R.create_element ~key:"command-selector" ~props:(object%js
    val onChangeCommand = fun command ->
      Lwt.ignore_result @@ I.(Context.execute instance (module B.Read_completion) command)
  end)
  P_command_selector.component

let component =
  let spec = R.component_spec
      ~constructor:(fun this _ ->
          this##.state := object%js
            val command = None
          end
        )
      (fun this ->
         let children =
           match this##.state##.command with
           | None -> [show_command_selector this##.props##.context]
           | Some C.Command.Class.Scanner_jump ->
             failwith "not implemented yet"
         in
         R.Dom.of_tag `div ~props:(R.element_spec () ~class_name:"sf-CommandPallet")
           ~children
      )
  in
  Component.make spec
