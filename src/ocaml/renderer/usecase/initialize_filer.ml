(** This module defines usecase to initialize filer. *)
module C = Sxfiler_renderer_core

module S = Sxfiler_renderer_service

type param' =
  { initial_location : string
  ; pos : C.Types.File_list_pos.t }

module Make (Service : S.Filer.S) : C.Usecase.S with type param = param' = struct
  type t = {param : param'}
  type param = param'

  let create param = {param}

  let execute t dispatcher =
    let module E = Sxfiler_core.Error in
    let name = C.Types.File_list_pos.to_string t.param.pos in
    try%lwt
      let%lwt res =
        match%lwt Service.make {initial_location = t.param.initial_location; name} with
        | Ok res ->
          Lwt.return res
        | Error `Already_exists -> (
            match%lwt Service.get {name} with
            | Ok res ->
              Lwt.return res
            (* this exception to help handling error to return unit from nested monad. *)
            | Error _ ->
              raise E.(Printf.sprintf "Not found: %s" name |> create |> to_exn) )
      in
      let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
      Lwt.return @@ DI.(Dispatcher.dispatch this C.Message.(Update_filer (t.param.pos, res)))
    with E.Error t ->
      Logs.app (fun m -> m "%s" E.(to_string t)) ;
      Lwt.return_unit
end
