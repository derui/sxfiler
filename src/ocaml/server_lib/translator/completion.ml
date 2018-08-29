open Sxfiler_core
module C = Sxfiler_completion.Domain
module T = Sxfiler_rpc.Types.Completion

module Item = struct
  open T.Item

  let to_yojson t : Yojson.Safe.json = `Assoc [("id", `String t.id); ("value", `String t.value)]

  let of_yojson (js : Yojson.Safe.json) : (t, string) result =
    try
      let open Yojson.Safe.Util in
      let id = js |> member "id" |> to_string and value = js |> member "value" |> to_string in
      Ok {id; value}
    with Yojson.Safe.Util.Type_error (s, _) -> Error s

  let to_domain t = {C.Item.id = t.id; value = t.value}
  let of_domain t = {id = t.C.Item.id; value = t.value}
end

module Candidate = struct
  open T.Candidate

  let to_yojson t : Yojson.Safe.json =
    `Assoc [("start", `Int t.start); ("length", `Int t.length); ("value", Item.to_yojson t.value)]

  let of_yojson js : (t, string) result =
    let open Yojson.Safe.Util in
    try
      let start = js |> member "start" |> to_int
      and length = js |> member "length" |> to_int
      and value = js |> member "value" in
      let open Result.Infix in
      Item.of_yojson value >>= fun value -> Ok {start; length; value}
    with Type_error (s, _) -> Error s

  let to_domain t = {C.Candidate.start = t.start; length = t.length; value = Item.to_domain t.value}
  let of_domain t = {start = t.C.Candidate.start; length = t.length; value = Item.of_domain t.value}
end
