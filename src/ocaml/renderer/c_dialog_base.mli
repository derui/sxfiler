module R = Reactjscaml
module Component : sig
  type props =
    < _open : bool Js.t Js.readonly_prop;
      keyHandler : (R.Event.Keyboard_event.t -> unit) Js.optdef Js.readonly_prop;
      title : Js.js_string Js.t Js.readonly_prop >
  type state = < opened : bool Js.readonly_prop >
  type spec = (props, state) R.Core.Component_spec.t
  val make :
    spec -> (props, state) R.Core.React.component
end

val component : (Component.props, Component.state) R.Core.React.component