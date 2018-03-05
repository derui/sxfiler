
type t = {
  shift: bool;
  ctrl: bool;
  meta: bool;
  key: string;
}

class type js = object
  method shift: bool Js.t Js.readonly_prop
  method ctrl: bool Js.t Js.readonly_prop
  method meta: bool Js.t Js.readonly_prop
  method key: Js.js_string Js.t Js.readonly_prop
end

let to_js t = object%js
  val shift = Js.bool t.shift
  val ctrl = Js.bool t.ctrl
  val meta = Js.bool t.meta
  val key = Js.string t.key
end

let of_js js = {
  shift = Js.to_bool js##.shift;
  ctrl = Js.to_bool js##.ctrl;
  meta = Js.to_bool js##.meta;
  key = Js.to_string js##.key;
}

type token =
  | Meta
  | Ctrl
  | Shift
  | Key of string

let empty = {
  shift = false;
  ctrl = false;
  meta = false;
  key = "";
}

let lexer_table = [
  (Some "M-", Meta);
  (Some "C-", Ctrl);
  (Some "S-", Shift);
]

let match_lexer_table seq =
  let result = List.find_opt (fun (token, _) ->
      match token with
      | Some tok -> tok = seq
      | None -> false
    ) lexer_table
  in
  match result with
  | None -> None
  | Some (_, typ) -> Some typ

let lex_combination seq =
  let token = ref "" in
  let types = ref [] in

  match seq with
  | "" -> !types
  | seq -> begin
      String.iter (fun c ->
          let temp_token = !token ^ (String.make 1 c) in
          match match_lexer_table temp_token with
          | None -> begin
              token := temp_token;
            end
          | Some typ -> begin
              token := "";
              types := typ :: !types
            end
        ) seq;

      List.rev (Key !token :: !types)
    end

let parse_sequence seq =
  let tokens = lex_combination seq in

  match tokens with
  | [] -> None
  | _ ->
    let k = List.fold_left (fun item -> function
        | Meta -> {item with meta = true}
        | Ctrl -> {item with ctrl = true}
        | Shift -> {item with shift = true}
        | Key key -> {item with key}
      ) empty tokens
    in
    if k.key = "" then None else Some k

let invalid_formats = [
  (fun seq -> seq = "");
  (fun seq -> String.length seq > 1 && String.index_opt seq '-' = Some 0);
]

let of_keyseq key =
  let key = String.trim key in
  if List.exists (fun f -> f key) invalid_formats then
    None
  else
    parse_sequence key

let to_keyseq t =
  let meta = if t.meta then "M-" else ""
  and shift = if t.shift then "S-" else ""
  and ctrl = if t.ctrl then "C-" else "" in
  meta ^ shift ^ ctrl ^ t.key

let () =
  Js.export_all (object%js
    method kbd key = let key = Js.to_string key in
      let t = of_keyseq key |> Js.Opt.option in
      Js.Opt.map t to_js
  end)
