(** This module declares dependencies for signature-only modules in {!Sxfiler_domain} and {!Sxfiler_workflow} *)

open Sxfiler_core
module R = Sxfiler_rpc
module D = Sxfiler_domain
module F = Sxfiler_workflow
module I = Sxfiler_infrastructure

(* Clock module to get current unix time *)
module Common_step = struct
  let now () = Option.get @@ Time.of_float @@ Unix.gettimeofday ()
end

module type Option' = sig
  val option : App_option.t
end

module type S = sig
  module System : Sxfiler_core.System.S

  module Client : R.Client.Instance

  module Server : R.Server.Instance

  module Mediator : R.Interaction_mediator.Instance

  module Completer : D.Completer.Instance

  module Step : sig
    val scan_location : F.Common_step.File_list.scan_location

    val demand_decision : F.Common_step.Interaction.demand_decision

    val load_configuration : F.Common_step.Configuration.load

    val save_configuration : F.Common_step.Configuration.save

    val copy_item : F.Common_step.Filer.copy_item

    val delete_item : F.Common_step.Filer.delete_item

    val move_item : F.Common_step.Filer.move_item

    val store_keymap : F.Common_step.Keymap.store_keymap

    val resolve_keymap : F.Common_step.Keymap.resolve_keymap

    val load_keymap : F.Common_step.Keymap.load_keymap

    val update_collection : F.Common_step.Completer.update_collection

    val provide_collection : F.Common_step.Completer.provide_collection

    val store_theme : F.Common_step.Theme.Store_theme.t

    val remove_theme : F.Common_step.Theme.Remove_theme.t

    val list_theme : F.Common_step.Theme.List_theme.t
  end

  module Work_flow : sig
    module Filer : sig
      val initialize : F.Filer.Initialize.work_flow

      val reload_all : F.Filer.Reload_all.work_flow

      val move_location : F.Filer.Move_location.work_flow

      val open_node : F.Filer.Open_node.work_flow

      val up_directory : F.Filer.Up_directory.work_flow

      val toggle_mark : F.Filer.Toggle_mark.work_flow

      val move : F.Filer.Move.work_flow

      val copy : F.Filer.Copy.work_flow

      val delete : F.Filer.Delete.work_flow
    end

    module Keymap : sig
      val add_key_binding : F.Keymap.Add_key_binding.work_flow

      val remove_key_binding : F.Keymap.Remove_key_binding.work_flow

      val reload : F.Keymap.Reload.work_flow
    end

    module Completer : sig
      val initialize : F.Completer.Initialize.work_flow

      val complete : F.Completer.Complete.work_flow
    end

    module Configuration : sig
      val update : F.Configuration.Update.work_flow
    end

    module Theme : sig
      val add : F.Theme.Add_theme.work_flow

      val remove : F.Theme.Remove_theme.work_flow

      val list : F.Theme.List_theme.work_flow
    end
  end

  val post_event : R.Event_handler.publish
  (** function to post event *)
end

module Id_generator_string = struct
  type id = string

  let state = Random.get_state ()

  let generate () = Uuidm.v4_gen state () |> Uuidm.to_string
end

module Id_generator_uuid = struct
  type id = Uuidm.t

  let state = Random.get_state ()

  let generate () = Uuidm.v4_gen state ()
end

let make (module Option' : Option') (module Completer : D.Completer.Instance) (module Ws_actor : I.Ws_actor.Instance) =
  let module Client = (val R.Client.make (module Id_generator_string) (module Ws_actor)) in
  let module Mediator = (val R.Interaction_mediator.make (module Client)) in
  let module Server = (val R.Server.make (module Ws_actor)) in
  ( module struct
    module System = Sxfiler_core.System.Real
    module Client = Client
    module Server = Server
    module Mediator = Mediator
    module Completer = Completer

    module Step = struct
      let scan_location = I.Filer_step.scan_location

      let demand_decision command = Mediator.(Mediator.require_action instance ~command)

      let load_configuration () = Global.Configuration_store.get ()

      let save_configuration = Global.Configuration_store.update

      let copy_item = I.Filer_step.copy_item

      let move_item = I.Filer_step.move_item

      let delete_item = I.Filer_step.delete_item

      let store_keymap = I.Keymap_step.store_keymap (module Global.Keymap)

      let resolve_keymap = I.Keymap_step.resolve_keymap (module Global.Keymap)

      let load_keymap = I.Keymap_step.load_keymap

      let update_collection = Global.Cached_collection.update

      let provide_collection = Global.Cached_collection.get

      let store_theme = I.Theme_step.store_theme (Path.of_string Option'.option.theme_dir |> Result.get_ok)

      let remove_theme = I.Theme_step.remove_theme (Path.of_string Option'.option.theme_dir |> Result.get_ok)

      let list_theme = I.Theme_step.list_theme (Path.of_string Option'.option.theme_dir |> Result.get_ok)
    end

    module Work_flow = struct
      module Filer = struct
        let initialize = F.Filer.initialize Global.Filer.get Step.scan_location

        let reload_all = F.Filer.reload_all Step.scan_location

        let move_location = F.Filer.move_location Common_step.now Step.scan_location

        let open_node = F.Filer.open_node Step.scan_location Common_step.now

        let up_directory = F.Filer.up_directory Step.scan_location Common_step.now

        let toggle_mark = F.Filer.toggle_mark

        let move = F.Filer.move Common_step.now Step.demand_decision Step.scan_location Step.move_item

        let copy = F.Filer.copy Common_step.now Step.demand_decision Step.scan_location Step.copy_item

        let delete = F.Filer.delete Common_step.now Step.scan_location Step.delete_item
      end

      module Keymap = struct
        let add_key_binding = F.Keymap.add_key_binding Step.resolve_keymap Step.store_keymap

        let remove_key_binding = F.Keymap.remove_key_binding Step.resolve_keymap Step.store_keymap

        let reload = F.Keymap.reload Step.load_keymap Step.store_keymap
      end

      module Completer = struct
        let initialize = F.Completer.initialize Step.update_collection

        let complete = F.Completer.complete Step.provide_collection (module Completer) F.Common_step.Completer.read
      end

      module Configuration = struct
        let update = F.Configuration.update Step.load_configuration Step.save_configuration
      end

      module Theme = struct
        let add = F.Theme.add_theme Step.store_theme

        let remove = F.Theme.remove_theme Step.list_theme Step.remove_theme

        let list = F.Theme.list_theme Step.list_theme
      end
    end

    let post_event = Event_handlers.setup_handlers (module Client)
  end : S )
