module FFI = Sxfiler_ffi

module File_stat = struct
  type t = {
    uuid: string;
    filename: string;
    link_path: string option;
    stat: FFI.Fs.stat_obj Js.t;
  }

  class type js = object
    method uuid: Js.js_string Js.t Js.readonly_prop
    method filename: Js.js_string Js.t Js.readonly_prop
    method stat: FFI.Fs.stat_obj Js.t Js.readonly_prop
    method link_path: Js.js_string Js.t Js.optdef Js.readonly_prop
  end

  let make ~filename ~link_path ~stat =
    let rnd = Random.State.make_self_init () in
    {
      uuid = Uuidm.v4_gen rnd () |> Uuidm.to_string;
      filename;
      link_path;
      stat
    }

  let to_js t = object%js
    val uuid = Js.string t.uuid
    val filename = Js.string t.filename
    val stat = t.stat
    val link_path = let link_path = Js.Optdef.option t.link_path  in
      Js.Optdef.map link_path Js.string
  end

  let of_js js = {
    uuid = Js.to_string js##.uuid;
    filename = Js.to_string js##.filename;
    stat = js##.stat;
    link_path = Js.Optdef.map (js##.link_path) Js.to_string |> Js.Optdef.to_option;
  }
end
