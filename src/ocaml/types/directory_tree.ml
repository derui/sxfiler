(** This module provides simple directory tree *)

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
