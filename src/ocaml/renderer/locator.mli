
open Sxfiler_renderer_core.Locator_intf

module type Main = S with type store = Sxfiler_renderer_store.App.Store.t

(** [make_store ()] gets initial store of application  *)
val make_store: unit -> Sxfiler_renderer_store.App.Store.t

module Make
    (Rpc:Sxfiler_renderer_core.Rpc_intf.Rpc)
    (Context: Sxfiler_renderer_core.Context.Instance)
  : Main
