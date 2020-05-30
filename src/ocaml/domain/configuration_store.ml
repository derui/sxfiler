module Key = struct
  type t = string [@@deriving show, eq]

  let from_list v = if List.length v = 0 then None else String.concat "." v |> Option.some

  let to_list v = String.split_on_char '.' v

  let of_string v = v
end

type t = Yojson.Basic.t [@@deriving show, eq]

let empty : t = `Assoc []

let filter_key key = function
  | `Assoc list ->
      let assoc = List.filter (fun (key', _) -> key <> key') list in
      `Assoc assoc
  | _ as v      -> v

let put ~key ~value t = filter_key key t |> Yojson.Basic.Util.combine (`Assoc [ (key, value) ])

let keys t = Yojson.Basic.Util.keys t

let get ~key t = match Yojson.Basic.Util.member key t with `Null -> None | _ as v -> Some v

let to_list t = Yojson.Basic.Util.to_assoc t |> List.map (fun (key, value) -> (Key.of_string key, value))

let of_json t = match t with `Assoc _ -> t | _ -> empty
