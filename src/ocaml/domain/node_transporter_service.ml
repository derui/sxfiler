(** this module defines interface to transport node between filers. No implementation that is
    default provides from this, should implement yourself. *)

(** suggest and waiter to get interaction from others. *)
type suggest = Node.t -> Task_interaction.Suggestion.t * Task_interaction.Reply.typ Lwt.t

module type S = sig
  val transport : suggest:suggest -> nodes:Node.t list -> _to:File_tree.t -> unit Lwt.t
  (** [transport ~suggest ~nodes ~_to] do move [nodes] to the location [_to].

      When module need user decision to the node, call [suggest] with it to get suggestion that
      tell user what want to. *)
end
