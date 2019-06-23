(** this module defines interface to replicate item between filers. No implementation that is
    default provides from this, should implement yourself. *)

(** suggest and waiter to get interaction from others. *)
type suggest = File_item.t -> Task_interaction.Suggestion.t * Task_interaction.Reply.typ Lwt.t

module type S = sig
  val replicate : suggest:suggest -> items:File_item.t list -> _to:File_list.t -> unit Lwt.t
  (** [replicate ~suggest ~items ~_to] replicate [items] to the location [_to].

      When module need user decision to the item, call [suggest] with it to get suggestion that
      tell user what want to. *)
end
