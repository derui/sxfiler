module Filer_copy_selected = struct
  type t =
    | Overwrite
    | Rename    of Common.Not_empty_string.t
  [@@deriving eq, show]
end

module Filer_move_selected = struct
  type t =
    | Overwrite
    | Rename    of Common.Not_empty_string.t
  [@@deriving eq, show]
end

module Filer_delete_selected = struct
  type t = Confirm [@@deriving eq, show]
end

type command =
  | Filer_copy   of File_item.t
  | Filer_move   of File_item.t
  | Filer_delete of File_item.t
[@@deriving eq, show]

type event =
  | Filer_copy_selected   of Filer_copy_selected.t
  | Filer_move_selected   of Filer_move_selected.t
  | Filer_delete_selected of Filer_delete_selected.t
  | Canceled
[@@deriving eq, show]
