module C = Sxfiler_common
module R = Jsoo_reactjs

module Component : sig
  type props =
    < dispatch : Dispatcher.t Js.readonly_prop;
      state : C.State.t Js.readonly_prop >
  type renderer = props Js.t -> R.Core.React.element Js.t
  val make : renderer -> (props, unit, unit) R.Core.React.component
end

val component : (Component.props, unit, unit) R.Core.React.component
