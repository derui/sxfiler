(** This module provides implementation of {!Sxfiler_domain.Item_replication_service.S}. *)

open Sxfiler_core
module D = Sxfiler_domain
module C = Sxfiler_server_core

module Item_name_set = Set.Make (struct
    type t = string

    let compare = Stdlib.compare
  end)

module Make
    (NS : Notification_service.S)
    (MF : Message_notification_factory.S)
    (PF : Progress_notification_factory.S) : D.Item_replication_service.S = struct
  module Log = (val C.Logger.make ["infra"; "item_transport"])

  let buffer_size = 1024

  let item_name_set file_list =
    let item_names_in_to =
      file_list.D.File_list.items
      |> List.map (fun v -> Path.basename v.D.File_item.full_path)
      |> List.to_seq
    in
    Item_name_set.empty |> Item_name_set.add_seq item_names_in_to

  let replicate ~suggest ~items ~_to =
    let name_set_in_to = item_name_set _to in
    Lwt_list.iter_s
      (fun item ->
         let name = Path.basename item.D.File_item.full_path in
         let to_location = _to.D.File_list.location in
         let source = Path.to_string item.full_path in
         let dest = Path.of_list [Path.to_string to_location; name] |> Path.to_string in
         let copy_file source dest =
           let%lwt ic = Lwt_io.open_file ~flags:[Unix.O_RDONLY] ~mode:Lwt_io.Input source in
           let%lwt oc =
             Lwt_io.open_file ~flags:[Unix.O_CREAT; Unix.O_WRONLY] ~mode:Lwt_io.Output dest
           in
           let rec copy_file' ic oc =
             let%lwt buf = Lwt_io.read ~count:buffer_size ic in
             if buf = "" then Lwt.return_unit
             else
               let%lwt () = Lwt_io.write oc buf in
               copy_file' ic oc
           in
           Lwt_io.with_close_connection (fun (ic, oc) -> copy_file' ic oc) (ic, oc)
         in
         let%lwt () =
           if Item_name_set.mem name name_set_in_to then
             let suggestion, interaction = suggest item in
             let%lwt () = NS.send ~typ:Task_notification.Need_interaction.typ suggestion in
             match%lwt interaction with
             | D.Task_interaction.Reply.Overwrite true -> copy_file source dest
             | Overwrite false -> Lwt.return_unit
             | Rename name ->
               let dest = Path.of_list [Path.to_string to_location; name] |> Path.to_string in
               copy_file source dest
           else copy_file source dest
         in
         Log.debug (fun m -> m "Copy file: [%s] -> [%s]" source dest) ;%lwt
         MF.create ~level:Message_notification.Info
           ~body:Printf.(sprintf "Copy file: [%s] -> [%s]" source dest)
         |> NS.send ~typ:Message_notification.notification_typ)
      items
end
