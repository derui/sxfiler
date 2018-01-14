module FFI = Sxfiler_ffi

module File_stat = struct
  type t = {
    filename: string;
    stat: FFI.Fs.stat_obj Js.t;
  }

  class type js = object
    method filename: Js.js_string Js.t Js.readonly_prop
    method stat: FFI.Fs.stat_obj Js.t Js.readonly_prop
  end

  let to_js t = object%js
    val filename = Js.string t.filename
    val stat = t.stat
  end

  let of_js js = {
    filename = Js.to_string js##.filename;
    stat = js##.stat
  }
end
