open Sxfiler_kbd

class type js = object
  method ctrl: bool Js.t Js.readonly_prop
  method meta: bool Js.t Js.readonly_prop
  method key: Js.js_string Js.t Js.readonly_prop
end

let to_js t = object%js
  val ctrl = Js.bool t.ctrl
  val meta = Js.bool t.meta
  val key = Js.string t.key
end

let of_js js = {
  ctrl = Js.to_bool js##.ctrl;
  meta = Js.to_bool js##.meta;
  key = Js.to_string js##.key;
}

let () =
  Js.export_all (object%js
    method kbd key = let key = Js.to_string key in
      let t = of_keyseq key |> Js.Opt.option in
      Js.Opt.map t to_js
  end)
