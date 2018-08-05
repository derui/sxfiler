
open Locator_abbrevs

include module type of struct include Locator_intf end

(** A signature for store instance to make locator instance. *)
module type Store_instance = sig
  val this: S.App.Store.t
end

(** [make_store ()] gets initial store of application  *)
val make_store: unit -> S.App.Store.t

module Make
    (Client:C.Rpc.Client)
    (Context: Sxfiler_renderer_core.Context.Instance)
    (Store:Store_instance)
  : S
