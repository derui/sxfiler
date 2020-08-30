open Sxfiler_core
module D = Sxfiler_domain
module F = Sxfiler_workflow
module T = Sxfiler_translator.Theme
module G = Sxfiler_generated
module NE = D.Common.Not_empty_string

type store_configuration = D.Theme.Configuration.t -> unit Lwt.t

module Theme_json = struct
  type t = {
    name : NE.t;
    description : NE.t option;
    colors : (NE.t * D.Theme.Color_code.t) list;
    base : NE.t option;
  }
end

module Theme_map = Map.Make (struct
  type t = NE.t

  let compare = NE.compare
end)

(* Deadly simple digraph *)
module Theme_graph = struct
  type edges = NE.t list Theme_map.t

  type t = {
    nodes : Theme_json.t Theme_map.t;
    edges : edges;
  }

  type error = Not_found_node

  let add_edge ({ nodes; edges } as v) theme =
    if (not & Theme_map.mem theme.Theme_json.name nodes) || Option.is_none theme.base then Ok v
    else
      let open Option.Infix in
      let v' =
        let* base = theme.base in
        let* edges' = Theme_map.find_opt base edges in
        Some { v with edges = Theme_map.add theme.name (base :: edges') edges }
      in
      Option.to_result ~none:Not_found_node v'

  let make nodes =
    let t =
      {
        nodes = List.fold_left (fun acc v -> Theme_map.add v.Theme_json.name v acc) Theme_map.empty nodes;
        edges = List.fold_left (fun acc v -> Theme_map.add v.Theme_json.name [] acc) Theme_map.empty nodes;
      }
    in
    List.fold_left (fun t theme -> match t with Error _ -> t | Ok t -> add_edge t theme) (Ok t) nodes

  (* traverse graph and return list that is inheritance of themes *)
  let traverse theme t =
    let rec traverse' lists theme =
      match Theme_map.find_opt theme t.edges with
      | None | Some [] -> lists
      | Some edges     ->
          if List.length edges > 1 then lists
          else
            let node = List.hd edges in
            traverse' (node :: lists) node
    in
    traverse' [] theme.Theme_json.name
end

type errors =
  | Require_member of string
  | Invalid_theme  of string

let option_from_list = function [] -> None | v :: _ -> Some v

let load_theme fname =
  try
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_file fname in
    let open Result.Infix in
    let* name =
      [ json ] |> filter_member "name" |> filter_string |> function
      | []     -> Error (Require_member "name")
      | v :: _ -> NE.make v |> Option.to_result ~none:(Require_member "name")
    in
    let description =
      [ json ] |> filter_member "description" |> filter_string |> option_from_list |> Option.bind ~f:NE.make
    in
    let colors =
      [ json ] |> filter_member "colors" |> filter_assoc |> option_from_list
      |> Option.map (fun colors ->
             List.fold_left
               (fun acc (key, value) ->
                 let key = NE.make key in
                 let code =
                   [ value ] |> filter_string |> option_from_list |> Option.bind ~f:D.Theme.Color_code.of_string
                 in
                 match (key, code) with None, _ | _, None -> acc | Some key, Some code -> (key, code) :: acc)
               [] colors)
      |> Option.value ~default:[]
    in
    let base = [ json ] |> filter_member "base" |> filter_string |> option_from_list |> Option.bind ~f:NE.make in

    Ok { Theme_json.name; description; colors; base }
  with _ -> Error (Invalid_theme fname)

let to_theme_map themes =
  List.fold_left (fun acc theme -> Theme_map.add theme.Theme_json.name theme acc) Theme_map.empty themes

let list_theme : Path.t -> D.Theme.Definition.t list Lwt.t =
 fun dir ->
  let dir' = Path.to_string dir in

  Lwt.catch
    (fun () ->
      let themes = Sys.readdir dir' |> Array.map (Path.join dir %> Path.to_string) in
      let themes =
        Array.fold_left
          (fun themes theme -> match load_theme theme with Error _ -> themes | Ok theme -> theme :: themes)
          [] themes
      in
      let theme_map = to_theme_map themes in
      let graph = Theme_graph.make & List.map snd & Theme_map.bindings theme_map in
      match graph with
      | Error _  -> Lwt.return []
      | Ok graph ->
          let to_definition theme definitions =
            let base = theme.Theme_json.base |> Option.bind ~f:(Fun.flip Theme_map.find_opt definitions) in
            let theme =
              D.Theme.Definition.make ?description:theme.Theme_json.description ~name:theme.name ~colors:theme.colors
                ?base ()
            in
            theme
          in

          let themes =
            Seq.fold_left
              (fun acc (_, theme) ->
                let edges = Theme_graph.traverse theme graph in
                List.fold_left
                  (fun acc node ->
                    let theme =
                      Theme_map.find_opt node theme_map
                      |> Option.map (Fun.flip to_definition acc)
                      |> Option.map (fun theme -> Theme_map.add node theme acc)
                    in
                    Option.value ~default:acc theme)
                  acc edges)
              Theme_map.empty (Theme_map.to_seq theme_map)
          in

          Lwt.return & List.map snd & Theme_map.bindings themes)
    (fun _ -> Lwt.return [])

let store_theme : store_configuration -> Path.t -> F.Common_step.Theme.Store_theme.t =
 fun store_configuration dir color_pairs base_theme ->
  let module NE = D.Common.Not_empty_string in
  let module S = F.Common_step.Theme.Store_theme in
  let%lwt themes = list_theme dir in

  let base_theme =
    Option.(
      base_theme >>= fun base_theme ->
      List.find_opt (fun theme -> NE.equal theme.D.Theme.Definition.name base_theme) themes)
  in
  let theme = D.Theme.Configuration.(make ~colors:color_pairs ?base:base_theme ()) in
  let%lwt () = store_configuration theme in
  Lwt.return_ok @@ D.Theme.Configuration.merge_color theme
