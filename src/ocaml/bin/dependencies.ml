(** This module declares dependencies for signature-only modules in {!Sxfiler_domain} and {!Sxfiler_usecase}  *)

module C = Sxfiler_server_core
module U = Sxfiler_usecase
module D = Sxfiler_domain
module I = Sxfiler_server_infra

module type S = sig
  module System : Sxfiler_core.System.S
  module Clock : D.Location_record.Clock
  module Key_map_repo : D.Key_map_repository.S
  module Condition_repo : D.Condition.Repository
  module Filer_repo : D.Filer.Repository
  module Filer_factory : D.Filer.Factory.S
  module Configuration_repo : D.Configuration.Repository
  module Completion_repo : D.Completion.Repository
  module Plan_repo : D.Plan.Repository
  module Plan_factory : D.Plan.Factory.S
  module Notification_factory : D.Notification.Factory
  module Notification_service : D.Notification_service.S
  module Node_transporter_service : D.Node_transporter_service.S
  module Location_scanner_service : D.Location_scanner_service.S
  module Node_trash_service : D.Node_trash_service.S

  module Usecase : sig
    module Keymap_get : U.Keymap.Get.S
    module Keymap_add_context : U.Keymap.Add_context.S
    module Keymap_delete_context : U.Keymap.Delete_context.S
    module Plan_reject : U.Plan.Reject.S
    module Plan_filer_make_move_plan : U.Plan_filer.Make_move_plan.S
    module Plan_filer_make_delete_plan : U.Plan_filer.Make_delete_plan.S
    module Configuration_get : U.Configuration.Get.S
    module Completion_setup : U.Completion.Setup.S
    module Completion_read : U.Completion.Read.S
    module Filer_make : U.Filer.Make.S
    module Filer_get : U.Filer.Get.S
    module Filer_move_parent : U.Filer.Move_parent.S
    module Filer_enter_directory : U.Filer.Enter_directory.S
  end
end

module Make (Conn : C.Rpc_connection.Instance) (Completer : D.Completer.Instance) : S = struct
  module System = Sxfiler_core.System.Real
  module Clock = Global.Clock
  module Key_map_repo : D.Key_map_repository.S = I.Key_map_repo.Make (Global.Keymap)
  module Condition_repo : D.Condition.Repository = I.Condition_repo.Make (Global.Condition)
  module Filer_repo : D.Filer.Repository = I.Filer_repo.Make (Global.Root)
  module Filer_factory = D.Filer.Factory.Make (I.Id_generator.Gen_random_string)
  module Configuration_repo : D.Configuration.Repository = I.Configuration_repo.Make (Global.Root)
  module Completion_repo = I.Completion_repo.Make (Global.Cached_source)
  module Plan_repo : D.Plan.Repository = I.Plan_repo.Make (Global.Root)
  module Plan_factory = D.Plan.Factory.Make (I.Id_generator.Gen_random_string)
  module Notification_factory : D.Notification.Factory = I.Notification_factory
  module Notification_service = I.Notification_service.Make (Conn)

  module Node_transporter_service =
    I.Node_transporter_service.Make (Notification_service) (Notification_factory)

  module Location_scanner_service : D.Location_scanner_service.S = I.Location_scanner_service
  module Node_trash_service = I.Node_trash_service

  module Usecase = struct
    module Keymap_get = U.Keymap.Get.Make (Condition_repo) (Key_map_repo)
    module Keymap_add_context = U.Keymap.Add_context.Make (Condition_repo) (Key_map_repo)
    module Keymap_delete_context = U.Keymap.Delete_context.Make (Condition_repo) (Key_map_repo)
    module Plan_reject = U.Plan.Reject.Make (Plan_repo)

    module Plan_filer_make_move_plan =
      U.Plan_filer.Make_move_plan.Make (Filer_repo) (Plan_factory) (Node_transporter_service)
        (Location_scanner_service)

    module Plan_filer_make_delete_plan =
      U.Plan_filer.Make_delete_plan.Make (Filer_repo) (Plan_factory) (Location_scanner_service)
        (Node_trash_service)

    module Configuration_get = U.Configuration.Get.Make (Configuration_repo)
    module Completion_setup = U.Completion.Setup.Make (Completion_repo)
    module Completion_read = U.Completion.Read.Make (Completion_repo) (Completer)

    module Filer_make =
      U.Filer.Make.Make (Configuration_repo) (Filer_factory) (Filer_repo)
        (Location_scanner_service)

    module Filer_get = U.Filer.Get.Make (Filer_repo)

    module Filer_move_parent =
      U.Filer.Move_parent.Make (Filer_repo) (Location_scanner_service) (Clock)

    module Filer_enter_directory =
      U.Filer.Enter_directory.Make (Filer_repo) (Location_scanner_service) (Clock)
  end
end