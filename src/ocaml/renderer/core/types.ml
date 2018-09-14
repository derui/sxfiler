(** {!Mode} defines type that is current mode of view. *)
module Mode = struct
  type t =
    | File_tree
    | Preview
    | Complete

  let all_modes = [File_tree; Preview; Complete]

  let to_context = function
    | File_tree -> "onFileTree"
    | Preview -> "onPreview"
    | Complete -> "onComplete"
end

module File_list_pos = struct
  type t =
    [ `Left
    | `Right ]

  let to_string = function `Left -> "left" | `Right -> "right"
end

type corrections = Sxfiler_rpc.Types.Node.t list
