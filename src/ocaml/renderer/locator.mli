
open Sxfiler_renderer_core.Locator_intf

module type Main = S with type store = Sxfiler_renderer_store.App.Store.t
                      and type message = Sxfiler_renderer_core.Message.t

module Make
    (Rpc:Sxfiler_renderer_core.Rpc_intf.Rpc)
    (Context: Sxfiler_renderer_core.Context_intf.Instance
     with type store = Sxfiler_renderer_store.App.Store.t
      and type message = Sxfiler_renderer_core.Message.t)
  : Main
