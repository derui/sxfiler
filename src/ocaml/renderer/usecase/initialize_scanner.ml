(** This module defines usecase to initialize scanner. *)
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

type param' = {
  initial_location: string;
  name: string;
}

module Make(Service:S.Scanner.S) : C.Usecase.S with type param = param' = struct
  type t = {
    param: param';
  }

  type param = param'

  let create param = {param}

  let execute t dispatcher =

    try%lwt
      let%lwt res =
        match%lwt Service.make {initial_location = t.param.initial_location;
                                name = t.param.name} with
        | Ok res -> Lwt.return res
        | Error `Already_exists -> begin
            match%lwt Service.get {name = t.param.name} with
            | Ok res -> Lwt.return res
            (* this exception to help handling error to return unit from nested monad. *)
            | Error _ -> raise_notrace Exit
          end
      in
      let module DI = (val dispatcher: C.Dispatcher_intf.Instance) in
      Lwt.return @@ DI.(Dispatcher.dispatch this C.Message.(Update_scanner res))
    with Exit ->
      Logs.app (fun m -> m "Not found: %s" t.param.name);
      Lwt.return_unit
end
