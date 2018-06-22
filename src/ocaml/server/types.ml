
type sort_type =
  | Sort_name
  | Sort_size
  | Sort_date

type file_id = string

module File_stat = struct
  (** Type of stat of file. Note: The value of *time (atime, ctime, mtime) fields has time value
      in term of milliseconds, not seconds.
  *)
  type t = {
    mode: int32;
    uid: int;
    gid: int;
    atime: int64;
    ctime: int64;
    mtime: int64;
    size: int64;
    is_directory: bool;
    is_file: bool;
    is_symlink: bool;
  }

  let make ~mode ~uid ~gid ~atime ~ctime ~mtime ~size ~is_directory ~is_file ~is_symlink =
    {
      mode;
      uid;
      gid;
      atime;
      ctime;
      mtime;
      size;
      is_directory;
      is_file;
      is_symlink;
    }

end

(** Node is a item in a file tree. *)
module Node = struct
  (** [link_path] will have target of the link if item pointed to full_path is *link. *)
  type t = {
    full_path: string;
    stat: File_stat.t;
    parent_directory: string;
    link_path: string option;
  }

  let equal v1 v2 = v1.full_path = v2.full_path

  let make ~full_path ~stat ~parent_directory ~link_path =
    {
      full_path;
      stat;
      parent_directory;
      link_path
    }
end

(** simple directory tree *)
module Directory_tree = struct
  (* directory has components of full path are separated with {!Filename.dir_sep}. *)
  type directory = string
  type t = Nil
         | Tree of directory * t list

  let empty = Nil

  (** {[make_tree ~root directories]} makes the tree of directories with root as [root].
      If posix filesystem is, root is [/]. If Windows filesystem, root is drive letter.
  *)
  let make_tree ~root directories =
    let same_component dir = function
      | Nil -> false
      | Tree (dir', _) -> dir = dir'
    in
    let rec make_tree' tree dir =
      match dir with
      | [] -> tree
      | head :: rest_paths -> begin
          match tree with
          | Nil -> make_tree' (Tree (head, [])) dir
          | Tree (current, children) -> begin
            match List.find_opt (same_component head) children with
              | None ->
                let child = make_tree' (Tree (head, [])) rest_paths in
                Tree (current, child :: children)
              | Some child -> make_tree' child rest_paths
          end
        end
    in
    let root = Tree (root, []) in
    List.fold_left make_tree' root directories

  (** {[walk tree ~f]} execute [f] each directories in [tree].  *)
  let walk tree ~f =
    let rec walk' parent f = function
      | Nil -> ()
      | Tree (current, children) ->
        let dir = Filename.(concat parent current) in
        f dir children;
        List.iter (walk' dir f) children
    in
    match tree with
    | Nil -> ()
    | Tree (root, children) ->
      f root children;
      List.iter (walk' root f) children
end

(** [Tree_snapshot] has snapshot of a directory in {!Directory_tree}.
    Snapshot has a absolute path of the directory and nodes in the directory.
*)
module Tree_snapshot = struct
  type t = {
    directory: string;
    nodes: Node.t list;
  }

  let make ~directory ~nodes = {directory; nodes}
end

module Snapshot_record = struct
  type t = {
    directory: string;
    timestamp: int64;
  }

  let make ~directory ~timestamp = {directory; timestamp}
end

module Snapshot_history = struct
  type t = {
    records: Snapshot_record.t list;
    max_records: int;
  }

  (** {[make ?max_records ()]} gets new history.
     Use default value of [max_records] is [100] if it did not give any value.
  *)
  let make ?(max_records=100) () = { records = []; max_records = max 0 max_records}

  let sort_by_timestamp = List.sort (fun a b -> Int64.compare
                                        a.Snapshot_record.timestamp
                                        b.Snapshot_record.timestamp)
  (** {[add_record t ~record]} makes new record and *)
  let add_record t ~record =
    let records = sort_by_timestamp @@ record :: t.records in
    if t.max_records < List.length records then
      {t with records = List.rev records |> List.tl |> List.rev}
    else
      {t with records = records}

end

module String_map = Map.Make(struct
    type t = string
    let compare = compare
  end)

(** {!Tree_stack} provides simple management of stack of snapshot and directory tree,
    these will be used to workspace of task.
*)
module Tree_stack = struct
  type data = Snapshot of Tree_snapshot.t
            | Directory_tree of Directory_tree.t
  type t = data list

  let empty = []

  let push ~data t = data :: t
  let pop = function
    | [] -> None
    | data :: rest -> Some (data, rest)
end

(** The type is key-value store for {!module:Tree_stack}. *)
type tree_stack_map = Tree_stack.t String_map.t

(** {!snapshot_history_map} is key-value store for {!module:Snapshot_history}. *)
type snapshot_history_map = Snapshot_history.t String_map.t
