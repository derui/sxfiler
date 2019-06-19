(** This module provides implementation of {!Sxfiler_domain.Node_transporter_service.S}. *)

open Sxfiler_core
module D = Sxfiler_domain
module C = Sxfiler_server_core

module Node_name_set = Set.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

module Make
    (NS : Notification_service.S)
    (MF : Message_notification_factory.S)
    (PF : Progress_notification_factory.S) : D.Node_transporter_service.S = struct
  module Log = (val C.Logger.make ["infra"; "node_transport"])

  let node_name_set file_tree =
    let node_names_in_to =
      file_tree.D.File_tree.nodes
      |> List.map (fun v -> Path.basename v.D.Node.full_path)
      |> List.to_seq
    in
    Node_name_set.empty |> Node_name_set.add_seq node_names_in_to

  let transport ~suggest ~nodes ~_to =
    let name_set_in_to = node_name_set _to in
    Lwt_list.iter_s
      (fun node ->
        let name = Path.basename node.D.Node.full_path in
        let to_location = _to.D.File_tree.location in
        let source = Path.to_string node.D.Node.full_path in
        let dest = Path.of_list [Path.to_string to_location; name] |> Path.to_string in
        let move dst = Lwt_unix.rename source dst in
        let%lwt () =
          if Node_name_set.mem name name_set_in_to then
            let suggestion, interaction = suggest node in
            let%lwt () = NS.send ~typ:Task_notification.Need_interaction.typ suggestion in
            match%lwt interaction with
            | D.Task_interaction.Reply.Overwrite true -> move dest
            | Overwrite false -> Lwt.return_unit
            | Rename name ->
                let dest = Path.of_list [Path.to_string to_location; name] |> Path.to_string in
                move dest
          else move dest
        in
        Log.debug (fun m -> m "Move file: [%s] from [%s]" dest source) ;%lwt
        MF.create ~level:Message_notification.Info
          ~body:Printf.(sprintf "Move file: [%s] from [%s]" dest source)
        |> NS.send ~typ:Message_notification.notification_typ )
      nodes
end
