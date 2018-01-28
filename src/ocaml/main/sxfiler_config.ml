
module Core = struct
  type t = {
    keymap: Sxfiler_key_map.key_map;
  }

  let default = {
    keymap = Sxfiler_key_map.empty;
  }

end

module Json_conf = struct
  type t = {
    keymap: Yojson.Safe.json;
  } [@@deriving yojson]

  let to_config v =
    let module U = Sxfiler_common.Util.Option in
    let open Sxfiler_common.Util.Option.Infix in
    let v = v.keymap in
    Yojson.Safe.Util.(to_option to_assoc v) >>= (fun v ->
        let v = List.filter (fun (name, value) ->
            match Yojson.Safe.Util.to_string_option value with
            | None -> false
            | Some _ -> true
          ) v
        in
        Some v
      ) >>= (fun v ->
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
        let module K = Sxfiler_key_map in
        Some {Core.keymap = List.fold_left (fun keymap (key, action) ->
            match Sxfiler_action.of_yojson action with
            | Error e -> Firebug.console##error ("Unknown action: " ^ e);  keymap
            | Ok action -> K.add_keymap ~key_map:keymap ~key ~action
          ) K.empty v
          }
      ) |> U.get ~default:Core.default
end

include Core

let exists path =
  let path = Js.string path in
  try
    let module M = Sxfiler_modules in
    M.fs##statSync path |> ignore;
    true
  with Js.Error err ->
    Firebug.console##error err;
    false

let load path =
  if not @@ exists path then default
  else
    let json = Yojson.Safe.from_file path in
    match Json_conf.of_yojson json with
    | Ok v -> Json_conf.to_config v
    | Error err -> Firebug.console##error err; default
