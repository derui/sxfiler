open Sxfiler_core
open Sxfiler_rpc.Types.Node
module D = Sxfiler_domain.Node

let to_yojson t =
  `Assoc [
    "name", `String t.name;
    "stat", File_stat.to_yojson t.stat;
    "parentDirectory", `String t.parent_directory;
    "linkPath", match t.link_path with
    | None -> `Null
    | Some v -> `String v;
  ]

let of_yojson js =
  let open Yojson.Safe.Util in
  try
    let name = js |> member "name" |> to_string
    and stat = js |> member "stat" |> File_stat.of_yojson
    and parent_directory = js |> member "parentDirectory" |> to_string
    and link_path = js |> member "linkPath" in
    let open Result.Infix in
    stat >>= fun stat ->
    let link_path = match link_path with
      | `Null -> None
      | _ -> Some (to_string link_path)
    in
    Ok {name;stat;link_path;parent_directory;}
  with Type_error (s, _) -> Error s

let of_domain t = {
  name = Path.to_string t.D.full_path;
  stat = File_stat.of_domain t.stat;
  parent_directory = Path.dirname t.D.full_path;
  link_path = t.link_path;
}
