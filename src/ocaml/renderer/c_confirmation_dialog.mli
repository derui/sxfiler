module C = Sxfiler_common
module R = Reactjscaml

module Component : sig
  type props =
    < content : Js.js_string Js.t Js.readonly_prop;
      dispatch : Key_dispatcher.t Js.readonly_prop;
      state : C.State.t Js.readonly_prop;
      title : Js.js_string Js.t Js.readonly_prop >
  type state = < confirmed : bool Js.readonly_prop >
  type spec = (props, state) R.Core.Component_spec.t
  val make : spec -> (props, state) R.Core.React.component
end

val component : (Component.props, Component.state) R.Core.React.component
