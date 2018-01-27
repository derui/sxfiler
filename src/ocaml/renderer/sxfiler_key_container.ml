module C = Sxfiler_common
module R = Reactjscaml

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method state: C.State.js Js.readonly_prop
      method dispatch: Sxfiler_key_dispatcher.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let key_handler props ev =
  let module D = Sxfiler_key_dispatcher in
  D.dispatch props##.dispatch ev

let component = Component.make (fun props ->
    R.Dom.of_tag `div
      ~props:R.Core.Element_spec.({
          (empty ()) with class_name = Some (Sxfiler_classnames.make ["fp-KeyContainer"]);
                          on_key_down = Some (key_handler props);
                          on_key_up = Some (key_handler props);
                          on_key_press = Some (key_handler props);

        })
      ~children:([| R.element ~props:(props :> Sxfiler_file_list.Component.props)
                      Sxfiler_file_list.component |])
  )
