(** This module will run tasks with task queue. *)

open Sxfiler_core
open Abbrev

module Log = (val I.Logger.make [ "interaction_observer" ])

include Interaction_mediator_intf

module Impl = struct
  (** only entry point to add task to task queue in this module. *)

  type t = { client : (module Client.Instance) }

  (** [add_task task] add [task] to mailbox of task accepter. *)
  let require_action t ~command =
    match command with
    | D.Interaction.Filer_copy item   ->
        let request = { G.Filer.CopyUserDecisionRequest.item = Tr.File_item.of_domain item |> Option.some } in
        let%lwt response =
          let module C = (val t.client) in
          C.(Client.call instance Client_command.Filer.copy_interaction request)
        in
        let event =
          let open Option.Infix in
          let* response = Result.to_option response in
          match response.G.Filer.CopyUserDecisionResponse.action with
          | G.Filer.Action.OVERWRITE -> Some D.Interaction.(Filer_copy_selected Filer_copy_selected.Overwrite)
          | RENAME                   ->
              let open Option.Infix in
              let* new_name = D.Common.Not_empty_string.make response.new_name in
              Some D.Interaction.(Filer_copy_selected (Filer_copy_selected.Rename new_name))
          | CANCEL                   -> Some D.Interaction.Canceled
        in
        Option.value event ~default:D.Interaction.Canceled |> Lwt.return
    | D.Interaction.Filer_move item   ->
        let request = { G.Filer.MoveUserDecisionRequest.item = Tr.File_item.of_domain item |> Option.some } in
        let%lwt response =
          let module C = (val t.client) in
          C.(Client.call instance Client_command.Filer.move_interaction request)
        in
        let event =
          let open Option.Infix in
          let* response = Result.to_option response in
          match response.G.Filer.MoveUserDecisionResponse.action with
          | G.Filer.Action.OVERWRITE -> Some D.Interaction.(Filer_move_selected Filer_move_selected.Overwrite)
          | RENAME                   ->
              let open Option.Infix in
              let* new_name = D.Common.Not_empty_string.make response.new_name in
              Some D.Interaction.(Filer_move_selected (Filer_move_selected.Rename new_name))
          | CANCEL                   -> Some D.Interaction.Canceled
        in
        Option.value event ~default:D.Interaction.Canceled |> Lwt.return
    | D.Interaction.Filer_delete item ->
        let request = { G.Filer.DeleteUserDecisionRequest.item = Tr.File_item.of_domain item |> Option.some } in
        let%lwt response =
          let module C = (val t.client) in
          C.(Client.call instance Client_command.Filer.delete_interaction request)
        in
        let event =
          let open Option.Infix in
          let* response = Result.to_option response in
          match response.G.Filer.DeleteUserDecisionResponse.confirmed with
          | true  -> Some D.Interaction.(Filer_delete_selected Filer_delete_selected.Confirm)
          | false -> Some D.Interaction.Canceled
        in
        Option.value event ~default:D.Interaction.Canceled |> Lwt.return
end

let make (module C : Client.Instance) =
  ( module struct
    module Mediator = Impl

    let instance = { Impl.client = (module C) }
  end : Instance )
