module C = Sxfiler_common
module R = Jsoo_reactjs

module Component : sig
  type props =
    < content : Js.js_string Js.t Js.readonly_prop;
      dispatch : Dispatcher.t Js.readonly_prop;
      onComplete : (unit -> C.Types.User_action.t) Js.readonly_prop;
      state : C.State.t Js.readonly_prop;
      title : Js.js_string Js.t Js.readonly_prop >
  type state = < confirmed : bool Js.readonly_prop >
  type spec = (props, state, unit) R.Core.Component_spec.t
  val make : spec -> (props, state, unit) R.Core.React.component
end

val component : (Component.props, Component.state, unit) R.Core.React.component
