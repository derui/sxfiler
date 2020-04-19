open Sxfiler_core

(** name of the bookmark *)
module Name = struct
  type t = Name of string [@@deriving show, eq, ord]

  let make v = Name v

  let value (Name v) = v
end

module Item = struct
  type t = {
    (* path of the node *)
    name : Name.t;
    path : Path.t;
  }
  [@@deriving show, eq]

  let make ~name ~path = { name; path }
end

module Item_map = struct
  include Map.Make (struct
    type t = Name.t

    let compare = Name.compare
  end)

  let show t = to_seq t |> Seq.map (Name.show % fst) |> List.of_seq |> String.concat ";" |> Printf.sprintf "%s"

  let pp fmt t = Format.fprintf fmt "%s" (show t)
end

type t = Item.t Item_map.t

let show = Item_map.show

let pp = Item_map.pp

let empty : t = Item_map.empty

let insert ~name ~path t =
  let item = Item.make ~name ~path in
  Item_map.add name item t

let remove name t = Item_map.remove name t

let items t = Item_map.to_seq t |> Seq.map snd |> List.of_seq

let to_collection t =
  Item_map.to_seq t
  |> Seq.map (fun (_, item) ->
         let id = Digest.string (Path.to_string ~env:`Unix item.Item.path) in
         Completer.Item.make ~id ~value:(Path.to_string item.path))
  |> List.of_seq
