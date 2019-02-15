(** This module provides implementation of {!Sxfiler_domain.Node_transporter_service.S}. *)

open Sxfiler_core
module D = Sxfiler_domain
module C = Sxfiler_server_core

(* get destination name of node with correction when transport it. *)
let dest_name node new_name =
  match new_name with None -> Path.basename node.D.Node.full_path | Some new_name -> new_name

module Make (NS : D.Notification_service.S) (Factory : D.Notification.Factory) :
  D.Node_transporter_service.S = struct
  module Log = (val C.Logger.make ["infra"; "node_transport"])

  let transport ?new_name ~node ~_to () =
    let name = dest_name node new_name in
    let source = Path.to_string node.D.Node.full_path in
    let to_location = _to.D.File_tree.location in
    let dest = Path.of_list [Path.to_string to_location; name] |> Path.to_string in
    let%lwt () = Sys.rename source dest |> Lwt.return in
    Log.debug (fun m -> m "Move file: [%s] from [%s]" dest source) ;%lwt
    Factory.create ~level:D.Notification.Level.Info
      ~body:D.Notification.(Message Printf.(sprintf "Move file: [%s] from [%s]" dest source))
    |> NS.send
end
