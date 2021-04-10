(** This module declares dependencies for signature-only modules in {!Sxfiler_domain} and {!Sxfiler_workflow} *)

open Sxfiler_core
module R = Sxfiler_rpc
module D = Sxfiler_domain
module F = Sxfiler_workflow
module I = Sxfiler_infrastructure

(* Clock module to get current unix time *)

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
    module Common : F.Common_step.Instance

    module Filer : F.Common_step.Filer.Instance

    module File_list : F.Common_step.File_list.Instance

    module Interaction : F.Common_step.Interaction.Instance

    module Configuration : F.Common_step.Configuration.Instance

    module Keymap : F.Common_step.Keymap.Instance

    module Completer : F.Common_step.Completer.Instance

    module Theme : F.Common_step.Theme.Instance
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
      module Common : F.Common_step.Instance = struct
        let now () = Option.get @@ Time.of_float @@ Unix.gettimeofday ()
      end

      module File_list = I.File_list_step.Instance
      module Filer = I.Filer_step.Instance (Global.Filer)

      module Interaction = struct
        let demand_decision command = Mediator.(Mediator.require_action instance ~command)
      end

      module Configuration = struct
        let load = Global.Configuration_store.get

        let save = Global.Configuration_store.update
      end

      module Keymap = I.Keymap_step.Instance (Global.Keymap)

      module Completer = struct
        let update_collection = Global.Cached_collection.update

        let provide_collection = Global.Cached_collection.get
      end

      module Theme =
        I.Theme_step.Instance
          (struct
            let theme_dir = Option'.option.App_option.theme_dir

            let theme_config_key =
              D.Configuration_store.Key.from_list [ "internal"; "configuration"; "theme" ] |> Option.get
          end)
          (Global.Configuration_store)
    end

    let post_event = Event_handlers.setup_handlers (module Client)
  end : S )
