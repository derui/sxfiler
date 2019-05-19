(** Notifier provides an ability to send notification what the task state changed *)
module type S = sig
  val need_interaction :
    suggestions:Task_interaction.Suggestion.typ list -> Task_types.id -> unit Lwt.t
  (** [need_interaction ~accept task_id] send a notification that the task needs user interaction *)

  val finished : Task_types.id -> unit Lwt.t
  (** [finished task_id] send a notification that the task finished *)
end
