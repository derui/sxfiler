(** This module defines component to manage {!Sxfiler_renderer_store.Viewer_context} that besides
    component life cycle.
 *)
module T = Sxfiler_types
module R = Jsoo_reactjs
module Be = Sxfiler_renderer_behavior
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateful (struct
    class type t = object
      method context: (module Context.Instance) Js.readonly_prop
      method viewerModule: C.Types.Viewer_module.t Js.readonly_prop
    end
  end)(struct
    type t = unit
  end)

let component =
  let spec = R.component_spec
      ~component_did_mount:(fun this ->
          let module Context = (val this##.props##.context : Context.Instance) in
          Context.(Context.execute instance (module Be.Enter_view) this##.props##.viewerModule)
        )
      ~component_will_unmount:(fun this ->
          let module Context = (val this##.props##.context : Context.Instance) in
          Context.(Context.execute instance (module Be.Leave_view) this##.props##.viewerModule)
        )
      (fun _ -> R.empty ())
  in
  Component.make spec
