(** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Core = struct
  type store = S.App.Store.t
  type t = {
    mutable store: S.App.Store.t;
    rpc: (module Rpc.Rpc);
    locator: (module C.Locator_intf.S);
  }

  let get_store {store;_} = store

  let subscribe t f = S.App.Store.subscribe t.store
      ~f:(fun _ -> f t.store)

  (** [execute instance param] execute behavior [instance] with [param]. *)
  let execute
      (type p)
      (type r)
      t
      behavior
      (param:p) =
    let module Be = (val behavior : C.Behavior_intf.S with type param = p and type result = r) in
    let behavior = Be.make t.locator in
    Be.execute behavior param

end

let make rpc locator =
  let module C = Sxfiler_renderer_core in
  let config = S.Config.(Store.make @@ State.make ())
  and viewer_stacks = S.Viewer_stacks.(Store.make @@ State.make (Const.scanner_1, Const.scanner_2))
  and layout = S.Layout.(Store.make @@ State.make ())
  and keymap = S.Keymap.(Store.make @@ State.make ()) in
  let state = S.App.State.make ~config ~layout ~viewer_stacks ~keymap in
  (module struct
    type store = S.App.Store.t
    module Context = Core
    let instance = {
      Core.store = S.App.Store.make state;
      rpc;
      locator;
    }
  end : C.Context_intf.Instance with type store = S.App.Store.t)

module type Instance = C.Context_intf.Instance with type store = S.App.Store.t
