(** This module declares dependencies for signature-only modules in {!Sxfiler_domain} and {!Sxfiler_usecase}  *)

module C = Sxfiler_server_core
module U = Sxfiler_usecase
module D = Sxfiler_domain
module I = Sxfiler_server_infra

module type S = sig
  module Key_map_repo : D.Key_map_repository.S
  module Condition_repo : D.Condition.Repository
  module Filer_repo : D.Filer.Repository
  module Configuration_repo : D.Configuration.Repository
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
    module Plan_move_nodes : U.Plan_filer.Make_move_plan.S
    module Plan_filer_make_delete_plan : U.Plan_filer.Make_delete_plan.S
  end
end

module Make (Conn : C.Rpc_connection.Instance) : S = struct
  module Key_map_repo : D.Key_map_repository.S = I.Key_map_repo.Make (Global.Keymap)
  module Condition_repo : D.Condition.Repository = I.Condition_repo.Make (Global.Condition)
  module Filer_repo : D.Filer.Repository = I.Filer_repo.Make (Global.Root)
  module Configuration_repo : D.Configuration.Repository = I.Configuration_repo.Make (Global.Root)
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

    module Plan_move_nodes =
      U.Plan_filer.Make_move_plan.Make (Filer_repo) (Plan_factory) (Node_transporter_service)
        (Location_scanner_service)

    module Plan_filer_make_delete_plan =
      U.Plan_filer.Make_delete_plan.Make (Filer_repo) (Plan_factory) (Location_scanner_service)
        (Node_trash_service)
  end
end
