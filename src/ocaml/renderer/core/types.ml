(** {!Mode} defines type that is current mode of view. *)
module Mode = struct
  type t =
    | File_tree

end

module File_list_pos = struct
  type t = [`Left | `Right]

  let to_string = function
    | `Left -> "left"
    | `Right -> "right"
end
