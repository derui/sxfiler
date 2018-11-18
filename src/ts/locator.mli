open Locator_abbrevs

include module type of struct
  include Locator_intf
end

(** A signature for store instance to make locator instance. *)
module type Store_instance = sig
  val this : S.App.Store.t
end

val make_store : unit -> S.App.Store.t
(** [make_store ()] gets initial store of application  *)

module Make
    (Client : C.Rpc_client.S)
    (Context : Sxfiler_renderer_core.Context.Instance)
    (Store : Store_instance) : S
