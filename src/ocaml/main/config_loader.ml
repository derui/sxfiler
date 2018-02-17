module Cm = Sxfiler_common
module Core = Sxfiler_common.Config
module N = Jsoo_node

module Json_conf = struct
  type key_maps = {
    file_list: Yojson.Safe.json;
    confirm_dialog: Yojson.Safe.json;
  } [@@deriving yojson]

  type t = {
    key_map: key_maps;
  } [@@deriving yojson]

  let key_map_of_json v =
    let open Minimal_monadic_caml.Option.Infix in
    Yojson.Safe.Util.(to_option to_assoc v) >>= (fun v ->
        let v = List.filter (fun (name, value) ->
            match Sxfiler_kbd.of_keyseq name with
            | None -> false
            | Some _ -> true
          ) v
                |> List.map (fun (name, value) ->
                    let name = match Sxfiler_kbd.of_keyseq name with
                      | None -> Sxfiler_kbd.empty
                      | Some t -> t
                    in (name, value)
                  ) in
        Some v
      ) >>= (fun v ->
        let module K = Cm.Key_map in
        Some (List.fold_left (fun key_map (key, action) ->
            let key = Sxfiler_kbd.to_keyseq key |> Js.string in
            match Cm.Key_bindable_action.of_yojson action with
            | Error err -> Firebug.console##error ("Unknown action " ^ err); key_map
            | Ok action -> K.add_key_map ~key_map:key_map ~key ~action
          ) K.empty v)
      )

  let to_config v =
    let module U = Sxfiler_common.Util.Option in
    {
      Core.key_maps = {
        Core.file_list = U.get ~default:Cm.Key_map.empty (key_map_of_json v.key_map.file_list);
        Core.confirm_dialog = U.get ~default:Cm.Key_map.empty (key_map_of_json v.key_map.confirm_dialog);
      }
    }
end

include Core

let is_file path =
  let module Fs = N.Fs in
  try
    match Fs.statSync path with
    | Ok stat -> not (Js.to_bool stat##isDirectory)
    | _ -> false
  with Js.Error _ ->
    false

let default_config_path = "default.json"
let load_from_file app_dir path =
  let path = N.Path.resolve [app_dir; path] in
  if not @@ is_file path then raise Not_found
  else
    let json = Yojson.Safe.from_file path in
    match Json_conf.of_yojson json with
    | Ok v -> Json_conf.to_config v
    | Error err -> Firebug.console##error err; raise Not_found

let load app_dir path =
  try
    load_from_file app_dir path
  with Not_found ->
    load_from_file app_dir default_config_path
