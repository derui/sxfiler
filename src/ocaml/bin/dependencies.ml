(** This module declares dependencies for signature-only modules in {!Sxfiler_domain} and
    {!Sxfiler_usecase} *)

module C = Sxfiler_server_core
module U = Sxfiler_usecase
module D = Sxfiler_domain
module I = Sxfiler_server_infra
module TR = Sxfiler_server_task.Runner_intf

module type S = sig
  module System : Sxfiler_core.System.S
  module Clock : D.Location_record.Clock
  module Key_map_repo : D.Key_map_repository.S
  module Condition_repo : D.Condition.Repository
  module Filer_repo : D.Filer.Repository
  module Filer_factory : D.Filer.Factory.S
  module Configuration_repo : D.Configuration.Repository
  module Completion_repo : D.Completion.Repository
  module Message_notification_factory : I.Message_notification_factory.S
  module Progress_notification_factory : I.Progress_notification_factory.S
  module Notification_service : I.Notification_service.S
  module Item_transporter_service : D.Item_transporter_service.S
  module Item_replication_service : D.Item_replication_service.S
  module Location_scanner_service : D.Location_scanner_service.S
  module Item_trash_service : D.Item_trash_service.S
  module Key_map_resolve_service : D.Key_map_resolve_service.S
  module Task_repo : D.Task.Repository
  module Task_factory : D.Task.Factory.S

  module Usecase : sig
    module Keymap_get : U.Keymap.Get.S
    module Keymap_reload : U.Keymap.Reload.S
    module Configuration_get : U.Configuration.Get.S
    module Completion_setup : U.Completion.Setup.S
    module Completion_read : U.Completion.Read.S
    module Filer_make : U.Filer.Make.S
    module Filer_get : U.Filer.Get.S
    module Filer_move_parent : U.Filer.Move_parent.S
    module Filer_enter_directory : U.Filer.Enter_directory.S
    module Filer_toggle_mark : U.Filer.Toggle_mark.S
    module Filer_move : U.Filer.Move.S
    module Filer_copy : U.Filer.Copy.S
    module Filer_delete : U.Filer.Delete.S
    module Filer_jump_location : U.Filer.Jump_location.S
    module Task_send_reply : U.Task.Send_reply.S
  end
end

module Make
    (Conn : C.Rpc_connection.Instance)
    (Completer : D.Completer.Instance)
    (Runner : TR.Instance) : S = struct
  module System = Sxfiler_core.System.Real
  module Clock = Global.Clock
  module Key_map_repo : D.Key_map_repository.S = I.Key_map_repo.Make (Global.Keymap)
  module Condition_repo : D.Condition.Repository = I.Condition_repo.Make (Global.Condition)
  module Notification_service = I.Notification_service.Make (C.Rpc_client.Make (Conn))
  module Filer_repo : D.Filer.Repository = I.Filer_repo.Make (Global.Root) (Notification_service)
  module Filer_factory : D.Filer.Factory.S = D.Filer.Factory.Make (I.Id_generator.Gen_uuid)

  module Configuration_repo : D.Configuration.Repository =
    I.Configuration_repo.Make (Global.Configuration)

  module Completion_repo = I.Completion_repo.Make (Global.Cached_source)

  module Message_notification_factory =
    I.Message_notification_factory.Make (I.Id_generator.Gen_uuid)

  module Progress_notification_factory =
    I.Progress_notification_factory.Make (I.Id_generator.Gen_uuid)

  module Key_map_resolve_service = I.Key_map_resolve_service

  module Item_transporter_service =
    I.Item_transporter_service.Make (Notification_service) (Message_notification_factory)
      (Progress_notification_factory)

  module Item_replication_service =
    I.Item_replication_service.Make (Notification_service) (Message_notification_factory)
      (Progress_notification_factory)

  module Location_scanner_service : D.Location_scanner_service.S = I.Location_scanner_service

  module Item_trash_service =
    I.Item_trash_service.Make (Notification_service) (Message_notification_factory)
      (Progress_notification_factory)

  module Task_repo = I.Task_repo.Make (Global.Root) (Runner)
  module Task_factory = D.Task.Factory.Make (I.Id_generator.Gen_uuid)

  module Usecase = struct
    module Keymap_get = U.Keymap.Get.Make (Condition_repo) (Key_map_repo)

    module Keymap_reload =
      U.Keymap.Reload.Make (Condition_repo) (Configuration_repo) (Key_map_repo)
        (Key_map_resolve_service)

    module Configuration_get = U.Configuration.Get.Make (Configuration_repo)
    module Completion_setup = U.Completion.Setup.Make (Completion_repo)
    module Completion_read = U.Completion.Read.Make (Completion_repo) (Completer)

    module Filer_make =
      U.Filer.Make.Make (Configuration_repo) (Filer_repo) (Filer_factory)
        (Location_scanner_service)

    module Filer_get = U.Filer.Get.Make (Filer_repo)

    module Filer_move_parent =
      U.Filer.Move_parent.Make (Filer_repo) (Location_scanner_service) (Clock)

    module Filer_jump_location =
      U.Filer.Jump_location.Make (Filer_repo) (Location_scanner_service) (Clock)

    module Filer_enter_directory =
      U.Filer.Enter_directory.Make (Filer_repo) (Location_scanner_service) (Clock)

    module Filer_toggle_mark = U.Filer.Toggle_mark.Make (Filer_repo)

    module Filer_move = U.Filer.Move.Make (struct
      module FR = Filer_repo
      module TF = Task_factory
      module TR = Task_repo
      module Scan = Location_scanner_service
      module Transport = Item_transporter_service
    end)

    module Filer_copy = U.Filer.Copy.Make (struct
      module FR = Filer_repo
      module TF = Task_factory
      module TR = Task_repo
      module Scan = Location_scanner_service
      module Replicate = Item_replication_service
    end)

    module Filer_delete = U.Filer.Delete.Make (struct
      module FR = Filer_repo
      module TF = Task_factory
      module TR = Task_repo
      module Scan = Location_scanner_service
      module Trash = Item_trash_service
    end)

    module Task_send_reply = U.Task.Send_reply.Make (Task_repo)
  end
end
