open Sxfiler_core

module Id = Common.Identity.Make (struct
  type t = string [@@deriving show, eq, ord]
end)

module Item = struct
  type t = {
    id : Id.t;
    full_path : Path.t;
    stat : File_stat.t;
  }
  [@@deriving show]

  let equal { id = p1; _ } { id = p2; _ } = Id.equal p1 p2

  let full_path { full_path; _ } = full_path

  let stat { stat; _ } = stat
end

type t =
  | Marked   of Item.t
  | Unmarked of Item.t
[@@deriving eq, show]

let id = function Marked { id; _ } -> id | Unmarked { id; _ } -> id

let item = function Marked v -> v | Unmarked v -> v

let make ~id ~full_path ~stat = Unmarked { id; full_path; stat }

let is_file' v = match v.Item.stat with { kind = File_stat.Kind.File; _ } -> true | _ -> false

let is_directory' v = match v.Item.stat with { kind = File_stat.Kind.Directory; _ } -> true | _ -> false

let is_symlink' v = match v.Item.stat with { kind = File_stat.Kind.Symlink _; _ } -> true | _ -> false

let apply_with_item f = function Marked v -> f v | Unmarked v -> f v

let is_directory = apply_with_item is_directory'

let is_file = apply_with_item is_file'

let is_symlink = apply_with_item is_symlink'

let mark = function Marked _ as v -> v | Unmarked v -> Marked v

let unmark = function Unmarked _ as v -> v | Marked v -> Unmarked v

type compare = t -> t -> int

let stat = apply_with_item (fun { stat; _ } -> stat)

let full_path = apply_with_item (fun { full_path; _ } -> full_path)

let compare_by = function
  | Types.Sort_type.Date ->
      fun v1 v2 ->
        let { File_stat.stat = v1_stat; _ } = stat v1 and { File_stat.stat = v2_stat; _ } = stat v2 in
        Time.compare v1_stat.mtime v2_stat.mtime
  | Types.Sort_type.Name ->
      fun v1 v2 ->
        let path1 = full_path v1 and path2 = full_path v2 in
        String.compare (Path.basename path1) (Path.basename path2)
  | Types.Sort_type.Size ->
      fun v1 v2 ->
        let { File_stat.stat = v1_stat; _ } = stat v1 and { File_stat.stat = v2_stat; _ } = stat v2 in
        File_stat.Size.compare v1_stat.size v2_stat.size
