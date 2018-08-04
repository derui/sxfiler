(** {!Mode} defines type that is current mode of view. *)
module Mode : sig
  type t =
    | Command
    | Completion
    | File_tree
end
