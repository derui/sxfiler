(** Completer provides simple completion interface via string. *)

(** The type of completer. *)
type t = {
  migemo: Migemocaml.Migemo.t;
}

type 'a candidate = {
  start: int;
  length: int;
  value: 'a
}

let make ~migemo = {migemo}

let is_some = function
  | Some _ -> true
  | None -> false

let read (type v) t ~input ~collection ~(stringify:(module Collection.Type with type t = v)) =
  let module S = (val stringify : Collection.Type with type t = v) in
  let regexp = Migemocaml.Migemo.query ~query:input t.migemo |> Re.Posix.compile_pat in
  let collection = List.map (fun v -> (S.to_string v, v)) collection in
  List.map (fun s -> (Re.exec_opt regexp @@ fst s, snd s)) collection
  |> List.filter (fun v -> is_some @@ fst v)
  |> List.map (function
      | (None, _) -> failwith "Invalid branch"
      | (Some group, v)  -> let start, length = Re.Group.offset group 0 in
        {start; length; value = v}
    )
