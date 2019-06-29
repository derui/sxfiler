open Sxfiler_core
(** This module provides implementation of trash service.*)

module C = Sxfiler_server_core
module D = Sxfiler_domain

(* delete whole directory *)
let rec delete_dir path =
  let paths = Sys.readdir path |> Array.to_list in
  let dirs = List.filter Sys.is_directory paths
  and files = List.filter Fun.(Sys.is_directory %> not) paths in
  let%lwt () = Lwt_list.iter_p (fun path -> Sys.remove path |> Lwt.return) files in
  let%lwt () = Lwt_list.iter_p delete_dir dirs in
  Unix.rmdir path |> Lwt.return

module Make
    (NS : Notification_service.S)
    (MF : Message_notification_factory.S)
    (PF : Progress_notification_factory.S) : D.Item_trash_service.S = struct
  module Log = (val C.Logger.make ["infra"; "item_transport"])

  let trash items =
    let total_count = List.length items in
    let counter_mutex = Lwt_mutex.create () and counter = ref 0 in
    let up_count () =
      Lwt_mutex.with_lock counter_mutex (fun () ->
          incr counter ;
          PF.create
            ~body:
              { Progress_notification.process = "delete"
              ; targeted = float_of_int total_count
              ; current = float_of_int !counter }
          |> NS.send ~typ:Progress_notification.notification_typ)
    in
    Lwt_list.iter_p
      (fun item ->
         let path = item.D.File_item.full_path |> Path.to_string in
         let%lwt () =
           match item.D.File_item.stat.is_directory with
           | true -> delete_dir path
           | false -> File.remove path |> Lwt.return
         in
         up_count () ;%lwt
         Log.debug (fun m -> m "Delete item: [%s]" path) ;%lwt
         MF.create ~level:Message_notification.Info ~body:Printf.(sprintf "Delete item: [%s]" path)
         |> NS.send ~typ:Message_notification.notification_typ)
      items
end
