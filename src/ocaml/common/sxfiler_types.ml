module FFI = Sxfiler_ffi

module File_stat = struct
  type t = {
    filename: string;
    link_path: string option;
    stat: FFI.Fs.stat_obj Js.t;
  }

  class type js = object
    method filename: Js.js_string Js.t Js.readonly_prop
    method stat: FFI.Fs.stat_obj Js.t Js.readonly_prop
    method link_path: Js.js_string Js.t Js.optdef Js.readonly_prop
  end

  let to_js t = object%js
    val filename = Js.string t.filename
    val stat = t.stat
    val link_path = let link_path = Js.Optdef.option t.link_path  in
      Js.Optdef.map link_path Js.string
  end

  let of_js js = {
    filename = Js.to_string js##.filename;
    stat = js##.stat;
    link_path = Js.Optdef.map (js##.link_path) Js.to_string |> Js.Optdef.to_option;
  }
end
