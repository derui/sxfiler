(** This module provides implementation of {!Sxfiler_domain.Node_replication_service.S}. *)

open Sxfiler_core
module D = Sxfiler_domain
module C = Sxfiler_server_core

(* get destination name of node with correction when transport it. *)
let dest_name node new_name =
  match new_name with None -> Path.basename node.D.Node.full_path | Some new_name -> new_name

module Make (NS : D.Notification_service.S) (Factory : D.Notification.Factory) :
  D.Node_replication_service.S = struct
  module Log = (val C.Logger.make ["infra"; "node_transport"])

  let buffer_size = 1024

  let replicate ?new_name ~node ~_to () =
    let name = dest_name node new_name in
    let source = Path.to_string node.D.Node.full_path in
    let to_location = _to.D.File_tree.location in
    let dest = Path.of_list [Path.to_string to_location; name] |> Path.to_string in
    let%lwt ic = Lwt_io.open_file ~flags:[Unix.O_RDONLY] ~mode:Lwt_io.Input source in
    let%lwt oc = Lwt_io.open_file ~flags:[Unix.O_CREAT; Unix.O_WRONLY] ~mode:Lwt_io.Output dest in
    let rec copy_file ic oc =
      let%lwt buf = Lwt_io.read ~count:buffer_size ic in
      if buf = "" then Lwt.return_unit
      else
        let%lwt () = Lwt_io.write oc buf in
        copy_file ic oc
    in
    let%lwt () = Lwt_io.with_close_connection (fun (ic, oc) -> copy_file ic oc) (ic, oc) in
    Log.debug (fun m -> m "Copy file: [%s] from [%s]" dest source) ;%lwt
    Factory.create ~level:D.Notification.Level.Info
      ~body:D.Notification.(Message Printf.(sprintf "Copy file: [%s] from [%s]" dest source))
    |> NS.send
end
