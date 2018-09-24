(** This module defines use case to make plan to move nodes between filers.  *)
open Sxfiler_core

module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service
module T = Sxfiler_rpc.Types

type param' = T.Plan.t

module Make (Service : S.Filer.S) : C.Usecase.S with type param = param' = struct
  type t = {param : param'}
  type param = param'

  let create param = {param}

  let execute t dispatcher =
    let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
    let move_nodes () =
      let%lwt message =
        try%lwt
          let%lwt () = Service.move_nodes {workbench_id = t.param.workbench_id} in
          Lwt.return C.Message.(Command Finished)
        with Error.Error t -> Lwt.return C.Message.(Raise_error t)
      in
      Lwt.return DI.(Dispatcher.dispatch this message)
    in
    let update_filer pos =
      match%lwt Service.get {name = C.Types.File_list_pos.to_string pos} with
      | Ok filer -> Lwt.return DI.(Dispatcher.dispatch this C.Message.(Update_filer (pos, filer)))
      | Error _ ->
        let message = C.Message.(Raise_error Error.(create "filer not found")) in
        Lwt.return DI.(Dispatcher.dispatch this message)
    in
    Lwt.(move_nodes () >>= fun () -> Lwt.join [update_filer `Left; update_filer `Right])
end
