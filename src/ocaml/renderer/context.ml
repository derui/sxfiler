(** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module type Instance = C.Context.Instance

type store = S.App.Store.t
type t = {
  mutable store: S.App.Store.t;
}
type config = S.App.Store.t

let create store =
  {
    store;
  }

let dispatcher t =
  C.Dispatcher.make_instance
    (module struct
      (* work around to avoid cyclic abbreviation. *)
      type instance = t
      type t = instance
      type config = unit
      let create () = t
      let dispatch t message = S.App.Store.dispatch t.store message
    end)
    ()

(** [execute instance param] execute behavior [instance] with [param]. *)
let execute t behavior =
  let module Be = (val behavior : C.Behavior_intf.Instance) in
  let module I = (val dispatcher t) in
  Be.(Behavior.execute this (module I))
