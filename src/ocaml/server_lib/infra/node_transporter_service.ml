(** This module provides implementation of {!Sxfiler_domain.Node_transporter_service.S}. *)

open Sxfiler_core
module D = Sxfiler_domain
module C = Sxfiler_server_core

module M = Map.Make (struct
    type t = string

    let compare = Stdlib.compare
  end)

(* get destination name of node with correction when transport it. *)
let dest_name node correction_map =
  match M.find_opt node.D.Node.id correction_map with
  | None -> Path.basename node.full_path
  | Some corrections ->
    let correct_name node =
      match List.find_opt (function D.Types.Correction.Name _ -> true) corrections with
      | None -> Path.basename node.D.Node.full_path
      | Some (Name name) -> name
    in
    correct_name node

module Make (NS : D.Notification_service.S) (Factory : D.Notification.Factory) = struct
  type location = Path.t

  let transport_process = "move"

  module Log = (val C.Logger.make ["infra"; "node_transport"])

  let transport ~nodes ~corrections ~_to =
    let correct_map =
      let module T = D.Types.Correction in
      corrections
      |> List.fold_left
        (fun map correction ->
           match M.find_opt correction.T.node_id map with
           | None -> M.add correction.node_id [correction.method_] map
           | Some list -> M.add correction.node_id (correction.method_ :: list) map )
        M.empty
    in
    let targeted = List.length nodes |> float_of_int in
    let number_of_nodes = Fun.(succ %> float_of_int) in
    nodes
    |> Lwt_list.iteri_s (fun i node ->
        let name = dest_name node correct_map in
        let source = Path.to_string node.full_path in
        let dest = Path.of_list [Path.to_string _to; name] |> Path.to_string in
        let%lwt () = Sys.rename source dest |> Lwt.return in
        Log.debug (fun m -> m "Move file: [%s] from [%s]" dest source) ;%lwt
        Factory.create ~level:D.Notification.Level.Info
          ~body:
            D.Notification.(
              Progress {process = transport_process; current = number_of_nodes i; targeted})
        |> NS.send )
end
