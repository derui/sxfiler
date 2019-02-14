(** Interface of repository for key map. *)
module type S = sig
  val store : Key_map.t -> unit Lwt.t
  (** [store keymap] save [keymap] to store anywhere. *)

  val resolve : unit -> Key_map.t Lwt.t
  (** [resolve ()] returns key map. Key map should be singleton in application. *)
end
