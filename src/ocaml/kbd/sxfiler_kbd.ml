
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

type prefix = Meta | Ctrl | Shift

let empty = {
  shift = false;
  ctrl = false;
  meta = false;
  key = "";
}

let meta_prefix = ("M-", Meta)
let ctrl_prefix = ("C-", Ctrl)
let shift_prefix = ("S-", Shift)

let parse_sequence seq =
  let rec parse_prefix seq prefixes = function
    | [] -> (seq ,prefixes)
    | (prefix, typ) :: rest -> begin
        let seq_len = String.length seq
        and prefix_len = String.length prefix in
        if seq_len = 1 then (seq, prefixes)
        else if String.sub seq 0 prefix_len = prefix then
          let next_seq = String.sub seq prefix_len (seq_len - prefix_len) in
          parse_prefix next_seq (typ :: prefixes) rest
        else
          parse_prefix seq prefixes rest
      end
  in
  let key, prefixes = parse_prefix seq [] [meta_prefix;ctrl_prefix;shift_prefix] in
  List.fold_left (fun item -> function
      | Meta -> {item with meta = true}
      | Ctrl -> {item with ctrl = true}
      | Shift -> {item with shift = true}
    ) {empty with key} prefixes

let invalid_formats = [
  (fun seq -> seq = "");
  (fun seq -> String.length seq > 1 && String.rindex_opt seq '-' = Some 0);
  (fun seq -> String.length seq > 1 && String.index_opt seq '-' = Some 0);
]

let of_keyseq key =
  let key = String.trim key in
  if List.exists (fun f -> f key) invalid_formats then
    None
  else
    Some (parse_sequence key)

module Export = struct
  class type _t = object
    method kbd: Js.js_string Js.t -> js Js.t Js.opt Js.meth
  end
  type t = _t Js.t
end

let () =
  Js.export_all (object%js
    method kbd key = let key = Js.to_string key in
      let t = of_keyseq key |> Js.Opt.option in
      Js.Opt.map t to_js
  end)
