(** Pathname is utility module more flexibility handling path and filename based on {!Filename} module.
    This module allows to handle pathname on windows and *nix platform do not change codebase.
*)

exception Empty_path

type component =
  | Comp_device of string
  | Comp_filename of string                        (* filename of path *)
  | Comp_parent                                    (* .. (Filename.parent_dir_name) *)
  | Comp_current                                   (* . (Filename.current_dir_name) *)
  | Comp_empty                                     (* / on *nix, "" on Windows *)

type t = {
  device: string;
  components: component list;
  resolved: bool;
}

type env = [`Unix | `Win]

let is_absolute path = not @@ Filename.is_relative path

let resolve_sep env =
  let sep_of_env = function
    | `Unix -> '/'
    | `Win -> '\\'
  in
  let f v = Option.fmap v ~f:sep_of_env in
  Option.get ~default:(fun () -> sep_of_env (if Sys.unix then `Unix else `Win)) @@ f env

(** [split_path_sep ?env path] splits from first separater '/' on *nix, or "\\" on Windows.
    If [path] does not have separator, return [(path, "")].

    Default separator is platform dependent separator such as [/] on *nix or [\\] on Windows when do not pass [sep].
*)
let split_path_sep ?env path =
  let length = String.length path in
  let sep = resolve_sep env in
  let is_sep c = c = sep in

  (* skip leading separators in path. *)
  let rec find_rest path pos =
    if pos < length then
      if is_sep path.[pos] then find_rest path (succ pos)
      else String.sub path pos (length - pos)
    else ""
  in

  let rec find_sep path pos =
    if pos < length then
      if is_sep path.[pos] then (String.sub path 0 pos, find_rest path pos)
      else find_sep path (succ pos)
    else
      (path, "")
  in
  find_sep path 0

(** [normalize_path ?env path] splits [path] with [env] as {!component}

    - "/" -> [Comp_device "/"]
    - "C:\\" -> [Comp_device "C:"]
    - "c/f" -> [Comp_filename "c";Comp_filename "f"]
    - "/a/b" -> [Comp_empty;Comp_filename "a";Comp_filename "b"]
*)
let normalize_path ?env sys path =
  let module S = (val sys: System.S) in
  let path_to_component = function
    | "" -> Comp_empty
    | _ as s when s = Filename.current_dir_name -> Comp_current
    | _ as s when s = Filename.parent_dir_name -> Comp_parent
    | _ as s -> Comp_filename s
  in

  let rec split_by_sep (path, rest) accum =
    if rest = "" then List.rev @@ path_to_component path :: accum
    else
      let accum = path_to_component path :: accum in
      split_by_sep (split_path_sep ?env rest) accum
  in

  let device_to_comp = function
    | "" -> Comp_device ""
    | _ as device -> Comp_device device
  in

  let initial, rest =
    if is_absolute path then
      let device, rest = split_path_sep ?env path in
      ([device_to_comp device], rest)
    else
      (* Use cwd *)
      let device, rest = split_path_sep ?env (S.getcwd ()) in
      (List.rev @@ split_by_sep (split_path_sep ?env rest) [device_to_comp device], path)
  in
  split_by_sep (split_path_sep ?env rest) initial

(** [of_string ?env path] converts [path] to Path object. *)
let of_string ?env sys path =
  if path = "" then raise Empty_path
  else
    let components = normalize_path ?env sys path in

    let rec construct_path comps t =
      match comps with
      | [] -> {t with components = List.rev t.components}
      | head :: rest -> begin match head with
          | Comp_device device -> construct_path rest {t with device}
          | _ as c -> construct_path rest {t with components = c :: t.components}
        end
    in

    construct_path components {device = ""; components = []; resolved = false}

(** [to_string ?env path] get a string representation of [path] *)
let to_string ?env path =
  let sep = resolve_sep env in

  let comps = List.map (function
      | Comp_device dev -> dev
      | Comp_current -> Filename.current_dir_name
      | Comp_parent -> Filename.parent_dir_name
      | Comp_empty -> ""
      | Comp_filename f -> f
    ) (Comp_device path.device :: path.components)
  in
  String.concat (String.make 1 sep) comps

let resolve path =
  if path.resolved then path
  else
    let rec resolve_relatives comps accum =
      match comps with
      | [] -> List.rev accum
      | head :: rest -> begin match head with
          (* This path do not enter now. *)
          | Comp_device _ -> failwith "Invalid component"
          | Comp_current | Comp_empty -> resolve_relatives rest accum
          | Comp_parent -> resolve_relatives rest (List.tl accum)
          | Comp_filename _ as a -> resolve_relatives rest (a :: accum)
        end
    in
    {path with resolved = true; components = resolve_relatives path.components []}

let equal t1 t2 =
  let t1' = to_string ~env:`Unix @@ resolve t1
  and t2' = to_string ~env:`Unix @@ resolve t2 in
  t1' = t2'
