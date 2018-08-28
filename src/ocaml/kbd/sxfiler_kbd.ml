type t =
  { ctrl : bool
  ; meta : bool
  ; key : string }

type token =
  | Meta
  | Ctrl
  | Key of string

let empty = {ctrl = false; meta = false; key = ""}
let make ?(ctrl = false) ?(meta = false) key = {key; ctrl; meta}
let key {key; _} = key
let has_meta {meta; _} = meta
let has_ctrl {ctrl; _} = ctrl
let lexer_table = [(Some "M-", Meta); (Some "C-", Ctrl)]

let match_lexer_table seq =
  let result =
    List.find_opt
      (fun (token, _) -> match token with Some tok -> tok = seq | None -> false)
      lexer_table
  in
  match result with None -> None | Some (_, typ) -> Some typ


let lex_combination seq =
  let token = ref "" in
  let types = ref [] in
  match seq with
  | "" ->
    !types
  | seq ->
    String.iter
      (fun c ->
         let temp_token = !token ^ String.make 1 c in
         match match_lexer_table temp_token with
         | None ->
           token := temp_token
         | Some typ ->
           token := "" ;
           types := typ :: !types )
      seq ;
    List.rev (Key !token :: !types)


let parse_sequence seq =
  let tokens = lex_combination seq in
  match tokens with
  | [] ->
    None
  | _ ->
    let k =
      List.fold_left
        (fun item -> function Meta -> {item with meta = true} | Ctrl -> {item with ctrl = true}
                            | Key key -> {item with key} )
        empty tokens
    in
    if k.key = "" then None else Some k


let invalid_formats =
  [(fun seq -> seq = ""); (fun seq -> String.length seq > 1 && String.index_opt seq '-' = Some 0)]


let of_keyseq key =
  let key = String.trim key in
  if List.exists (fun f -> f key) invalid_formats then None else parse_sequence key


let to_keyseq t =
  let meta = if t.meta then "M-" else "" and ctrl = if t.ctrl then "C-" else "" in
  meta ^ ctrl ^ t.key
