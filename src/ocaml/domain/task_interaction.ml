open Task_types

module Reply = struct
  type typ =
    | Overwrite of bool
    | Rename of string
  [@@deriving show, eq]

  type t =
    { task_id : id
    ; reply : typ }
  [@@deriving show, eq]
end

module Suggestion = struct
  type typ =
    | Overwrite
    | Rename
  [@@deriving show, eq]

  type t =
    { task_id : id
    ; item_name : string
    ; suggestions : typ list }
  [@@deriving show, eq]
end
