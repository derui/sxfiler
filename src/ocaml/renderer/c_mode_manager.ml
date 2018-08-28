(** This module handles mode activation *)

module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module SI = Sxfiler_renderer_service_impl
module U = Sxfiler_renderer_usecase

let activate_mode (module L : Locator.S) =
  let module Ctx = (val L.context) in
  let module Service = SI.Keymap.Make ((val L.client)) in
  let activation =
    C.Usecase.make_instance (module U.Activate_mode.Make (Service)) ~param:C.Types.Mode.Complete
  in
  Lwt.ignore_result
    (let%lwt () = Lwt_js.yield () in
     Ctx.(Context.execute this activation))


let t =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method locator : (module Locator.S) Js.readonly_prop
          end
      end )
    ~spec:
      R.(
        component_spec
          ~initial_custom:(fun _ _ -> object%js end)
          ~initial_state:(fun _ _ -> object%js end)
          ~should_component_update:(fun this new_props _ ->
              let module Current_loc = (val this##.props##.locator : Locator.S) in
              let module New_loc = (val new_props##.locator : Locator.S) in
              let to_ws store =
                S.Workspace.Store.get @@ S.App.State.workspace @@ S.App.Store.get store
              in
              let current_state = to_ws Current_loc.store in
              let new_state = to_ws New_loc.store in
              not @@ S.Workspace.State.equal current_state new_state )
          ~component_did_update:(fun this _ _ -> activate_mode this##.props##.locator)
          (fun _ -> R.empty ()))
