(** This behavior makes initialization to application. *)
module T = Sxfiler_domain
module C = Sxfiler_renderer_core

type t = {
  rpc: (module C.Rpc_intf.Rpc);
  source_class: T.Completion.Source_class.t;
  collection: T.Completion.collection;
  completer_id : string;
}

type param = T.Completion.Source_class.t * T.Completion.collection * string
type config = (module C.Locator_intf.S)

let create locator (cls, collection, completer_id) =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = L.rpc;
   source_class = cls;
   collection;
   completer_id;
  }

let execute t dispatcher  =

  let module RI = Sxfiler_rpc in
  C.Rpc.Client.request t.rpc (module C.Api.Completion.Setup_sync)
    (Some {RI.Completion.Setup_sync.source = t.collection})
    (function
      | Error _ -> ()
      | Ok _ ->
        let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
        let message = C.Message.(Completion (Setup (t.source_class, t.completer_id))) in
        D.(Dispatcher.dispatch this message)
    )
