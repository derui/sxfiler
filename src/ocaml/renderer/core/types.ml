(** {!Mode} defines type that is current mode of view. *)
module Mode = struct
  type content =
    | File_tree
    | Preview

  type t =
    | Content of content
    | Complete

  let to_context = function
    | Content File_tree -> "onFileTree"
    | Content Preview -> "onPreview"
    | Complete -> "onComplete"

  let others = function
    | Content File_tree -> [Complete; Content Preview]
    | Content Preview -> [Complete; Content File_tree]
    | Complete -> [Content File_tree; Content Preview]
end

module File_list_pos = struct
  type t =
    [ `Left
    | `Right ]

  let to_string = function `Left -> "left" | `Right -> "right"
end
