(** Path is utility module more flexibility handling path and filename based on {!Filename} module. This module allows
    to handle pathname on windows and *nix platform do not change codebase. *)

open Fun.Infix

type error = Empty_path

type component =
  | Comp_filename of string
  (* filename of path *)
  | Comp_parent
  (* .. (Filename.parent_dir_name) *)
  | Comp_current
  (* . (Filename.current_dir_name) *)
  | Comp_empty

(* / on *nix, "" on Windows *)

type t = {
  root : string option;
  components : component list;
  resolved : bool;
}

type env =
  [ `Unix
  | `Win
  ]

let equal p1 p2 =
  match (p1.resolved, p2.resolved) with
  | true, false | false, true -> false (* can not compare between resolved and unresolved *)
  | _, _                      ->
      let drop_current_comp = List.filter (function Comp_current -> false | _ -> true) in
      p1.root = p2.root && drop_current_comp p1.components = drop_current_comp p2.components

let resolve_sep env =
  let sep_of_env = function `Unix -> '/' | `Win -> '\\' in
  let f v = Option.map sep_of_env v in
  match f env with None -> sep_of_env (if Sys.unix then `Unix else `Win) | Some v -> v

(** [split_path_sep ?env path] splits from first separator '/' on *nix, or "\\" on Windows.

    If path does not have separator, return tuple that first element is [path], and second is empty string.

    Default separator is platform dependent separator such as "/" on *nix or "\\" on Windows when do not pass separator. *)
let split_path_sep ?env path =
  let length = String.length path in
  let sep = resolve_sep env in
  let is_sep c = c = sep in
  (* skip leading separators in path. *)
  let rec find_rest path pos =
    if pos < length then if is_sep path.[pos] then find_rest path (succ pos) else String.sub path pos (length - pos)
    else ""
  in
  let rec find_sep path pos =
    if pos < length then
      if is_sep path.[pos] then (String.sub path 0 pos, find_rest path pos) else find_sep path (succ pos)
    else (path, "")
  in
  find_sep path 0

(** [normalize_path ?env path] splits [path] with [env] as {!component}.

    - {[ "/" => [ Comp_empty ] ]}
    - {[ "C:\\" => [ Comp_filename "C:" ] ]}
    - {[ "c/f" => [ Comp_current; Comp_filename "c"; Comp_filename "f" ] ]}
    - {[ "/a/b" => [ Comp_empty; Comp_filename "a"; Comp_filename "b" ] ]} *)
let normalize_path ?env path =
  let path_to_component = function
    | "" -> Comp_empty
    | _ as s when s = Filename.current_dir_name -> Comp_current
    | _ as s when s = Filename.parent_dir_name -> Comp_parent
    | _ as s -> Comp_filename s
  in
  let rec split_by_sep (path, rest) accum =
    if rest = "" then List.rev & path_to_component path :: accum
    else
      let accum = path_to_component path :: accum in
      split_by_sep (split_path_sep ?env rest) accum
  in
  let rec remove_duplicated_cwd list cwd_continue accum =
    match list with
    | []                          -> List.rev accum
    | (Comp_current as v) :: rest ->
        if cwd_continue then remove_duplicated_cwd rest cwd_continue accum
        else remove_duplicated_cwd rest true (v :: accum)
    | head :: rest                -> remove_duplicated_cwd rest false (head :: accum)
  in
  let initial, rest = ([], path) in
  let components = split_by_sep (split_path_sep ?env rest) initial in
  remove_duplicated_cwd components false []

(* Resolve root of components if pattern found. *)
let find_root ?env components =
  let env = match env with None -> if Sys.unix then `Unix else `Win | Some v -> v in
  let is_drive_letter str =
    String.length str = 2
    && String.contains_from str 1 ':'
    && match str.[0] with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  in
  match (components, env) with
  | Comp_empty :: _, `Unix -> (Some "/", components)
  | Comp_filename v :: rest, `Win when is_drive_letter v ->
      let sep = resolve_sep (Some env) in
      let sep = String.make 1 sep in
      (Some (v ^ sep), rest)
  | _ -> (None, components)

(** [of_string ?env path] converts [path] to Path object. *)
let of_string ?env path =
  if path = "" then Error Empty_path
  else
    let components = normalize_path ?env path in
    let root, components = find_root ?env components in
    Ok { root; components; resolved = false }

let resolve ?env sys path =
  if path.resolved then path
  else
    let module S = (val sys : System.S) in
    let rec resolve_relatives comps accum =
      match comps with
      | []           -> List.rev accum
      | head :: rest -> (
          match head with
          | Comp_current         -> resolve_relatives rest accum
          | Comp_empty           -> resolve_relatives rest accum
          | Comp_parent          -> resolve_relatives rest (List.tl accum)
          | Comp_filename _ as a -> resolve_relatives rest (a :: accum))
    in
    let root, components =
      match path.root with
      | None   -> (
          match of_string ?env & S.getcwd () with
          | Error _ -> (path.root, path.components)
          | Ok cwd  -> (cwd.root, cwd.components @ path.components))
      | Some _ -> (path.root, path.components)
    in
    { root; resolved = true; components = resolve_relatives components [] }

(** [to_string ?env path] get a string representation of [path] *)
let to_string ?env path =
  let sep = resolve_sep env in
  let comps =
    List.filter (function Comp_empty -> false | _ -> true) path.components
    |> List.map (function
         | Comp_current    -> Filename.current_dir_name
         | Comp_parent     -> Filename.parent_dir_name
         | Comp_empty      -> ""
         | Comp_filename f -> f)
  in
  let concatted = String.concat (String.make 1 sep) comps in
  match path.root with None -> concatted | Some v -> v ^ concatted

let of_list ?env paths =
  match paths with
  | [] -> Error Empty_path
  | _  ->
      let sep = resolve_sep env in
      let path = String.concat (String.make 1 sep) paths in
      of_string ?env path

let basename path =
  let components = List.rev path.components in
  match components with [] -> "" | Comp_filename fname :: _ -> fname | _ -> ""

let dirname_as_path path =
  match List.rev path.components with
  | []              -> path
  | _ as components -> { path with components = List.rev & List.tl components }

let dirname ?env path =
  let path' = dirname_as_path path in
  match to_string ?env path' with "" -> Filename.current_dir_name | _ as v -> v

let pp fmt t =
  let path = to_string ~env:`Unix t in
  Format.fprintf fmt "%s" path

let show_error = function Empty_path -> "Empty_path"

let pp_error fmt e = Format.fprintf fmt "%s" & show_error e

let join ?env path place =
  let place' = of_string ?env place in
  match place' with
  | Ok v    ->
      (* If place is full path, it has Comp_empty to indicate it is full path. *)
      let rootless_comps = List.filter (function Comp_empty -> false | _ -> true) v.components in
      { path with components = List.concat [ path.components; rootless_comps ] }
  | Error _ -> path
