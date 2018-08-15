(** This behavior makes initialization to application. *)
module Co = Sxfiler_completion.Domain
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

type param = Co.collection * string

module Make(Service:S.Completion.S) : C.Usecase.S with type param = param = struct
  type t = {
    collection: Co.collection;
    completer_id : string;
  }

  type param = Co.collection * string

  let create (collection, completer_id) =
    {collection;
     completer_id;
    }

  let execute t dispatcher  =
    let%lwt () = Service.setup {source = t.collection} in
    let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
    let message = C.Message.(Completion (Setup (t.completer_id))) in
    Lwt.return @@ D.(Dispatcher.dispatch this message)
end