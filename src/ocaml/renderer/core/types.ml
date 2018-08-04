(** {!Mode} defines type that is current mode of view. *)
module Mode = struct
  type t =
    | Command
    | Completion
    | File_tree

end
