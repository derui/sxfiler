(** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
module C = Sxfiler_renderer_core

module S = Sxfiler_renderer_store

module type Instance = C.Context.Instance

type store = S.App.Store.t
type t = {store : S.App.Store.t}
type config = S.App.Store.t

let create store = {store}

let dispatcher t =
  C.Dispatcher.make_instance
    ( module struct
      (* work around to avoid cyclic abbreviation. *)
      type instance = t
      type t = instance
      type config = unit

      let create () = t
      let dispatch t message = S.App.Store.dispatch t.store message
    end )
    ()

(** [execute instance usecase] execute usecase [usecase]. *)
let execute t usecase =
  let module U = (val usecase : C.Usecase_intf.Instance) in
  let module I = (val dispatcher t) in
  U.(Usecase.execute this (module I))
