(** {!Mode} defines type that is current mode of view. *)
module Mode = struct
  type t =
    | File_tree
    | Complete

  let to_context = function
    | File_tree -> "onFileTree"
    | Complete -> "onComplete"

  let others = function
    | File_tree -> [Complete]
    | Complete -> [File_tree]

end

module File_list_pos = struct
  type t = [`Left | `Right]

  let to_string = function
    | `Left -> "left"
    | `Right -> "right"
end
