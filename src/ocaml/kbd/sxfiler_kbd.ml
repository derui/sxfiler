
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

let meta_prefix = (Str.regexp "^M-", Meta)
let ctrl_prefix = (Str.regexp "^C-", Ctrl)
let shift_prefix = (Str.regexp "^S-", Shift)

let parse_sequence seq =
  let rec parse_prefix seq prefixes = function
    | [] -> (seq ,prefixes)
    | (prefix, typ) :: rest -> begin
        if Str.string_match prefix seq 0 then
          let next_seq = Str.replace_first prefix seq "" in
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
  Str.regexp "^.+-$";
  Str.regexp "^-.+$";
  Str.regexp "^[^MCS].+-";
]

let of_keyseq key =
  if List.exists (fun reg -> Str.string_match reg key 0) invalid_formats then
    None
  else
    Some (parse_sequence key)

module Export = struct
  class type _t = object
    method kbd: Js.js_string Js.t -> js Js.t Js.opt Js.meth
  end
end

let () =
  Js.export_all (object%js
    method kbd key = let key = Js.to_string key in
      let t = of_keyseq key |> Js.Opt.option in
      Js.Opt.map t to_js
  end)
