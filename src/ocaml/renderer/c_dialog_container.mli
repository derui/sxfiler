module C = Sxfiler_common
module R = Reactjscaml

module Component : sig
  type props =
    < dispatch : Key_dispatcher.t Js.readonly_prop;
      state : C.State.t Js.readonly_prop >
  type renderer = props Js.t -> R.Core.React.element Js.t
  val make :
    renderer -> (props, unit) R.Core.React.component
end

val component : (Component.props, unit) R.Core.React.component
