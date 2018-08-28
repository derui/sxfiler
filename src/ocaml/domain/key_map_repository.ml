(** Interface of repository for key map. *)
module type S = sig
  type value

  val store : value Key_map.t -> unit Lwt.t
  (** [store keymap] save [keymap] to store anywhere. *)

  val resolve : unit -> value Key_map.t Lwt.t
  (** [resolve ()] returns key map. Key map should be singleton in application. *)
end
