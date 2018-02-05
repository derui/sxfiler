module FFI = Sxfiler_ffi

type current_cursor = int

module File_stat = struct
  type t = {
    id: string;
    filename: string;
    link_path: string option;
    stat: FFI.Fs.stat_obj Js.t;
  }

  class type js = object
    method id: Js.js_string Js.t Js.readonly_prop
    method filename: Js.js_string Js.t Js.readonly_prop
    method stat: FFI.Fs.stat_obj Js.t Js.readonly_prop
    method link_path: Js.js_string Js.t Js.optdef Js.readonly_prop
  end

  let equal v1 v2 = v1.id = v2.id

  let make ~filename ~link_path ~stat =
    let digest = Digest.string filename |> Digest.to_hex in
    {
      id = digest;
      filename;
      link_path;
      stat
    }

  let to_js t = object%js
    val id = Js.string t.id
    val filename = Js.string t.filename
    val stat = t.stat
    val link_path = let link_path = Js.Optdef.option t.link_path  in
      Js.Optdef.map link_path Js.string
  end

  let of_js js = {
    id = Js.to_string js##.id;
    filename = Js.to_string js##.filename;
    stat = js##.stat;
    link_path = Js.Optdef.map (js##.link_path) Js.to_string |> Js.Optdef.to_option;
  }
end
