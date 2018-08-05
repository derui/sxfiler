
open Sxfiler_renderer_core.Locator_intf

module type Main = S with type store = Sxfiler_renderer_store.App.Store.t

(** A signature for store instance to make locator instance. *)
module type Store_instance = sig
  type t
  val instance: t
end

(** [make_store ()] gets initial store of application  *)
val make_store: unit -> Sxfiler_renderer_store.App.Store.t

module Make
    (Rpc:Sxfiler_renderer_core.Rpc_intf.Rpc)
    (Context: Sxfiler_renderer_core.Context.Instance)
    (Store:Store_instance with type t := Sxfiler_renderer_store.App.Store.t)
  : Main
