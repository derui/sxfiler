(** this module defines interface to transport item between filers. No implementation that is
    default provides from this, should implement yourself. *)

type suggest = File_item.t -> Task_interaction.Suggestion.t * Task_interaction.Reply.typ Lwt.t
(** suggest and waiter to get interaction from others. *)

module type S = sig
  val transport : suggest:suggest -> items:File_item.t list -> _to:File_list.t -> unit Lwt.t
  (** [transport ~suggest ~items ~_to] do move [items] to the location [_to].

      When module need user decision to the item, call [suggest] with it to get suggestion that tell
      user what want to. *)
end
