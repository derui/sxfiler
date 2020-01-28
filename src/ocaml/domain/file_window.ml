open Sxfiler_core

type left_side

type right_side

type free

let show_left_side _ = "left_side"

let pp_left_side fmt v = Format.fprintf fmt "%s" (show_left_side v)

let equal_left_side _ _ = true

let show_right_side _ = "right_side"

let pp_right_side fmt v = Format.fprintf fmt "%s" (show_right_side v)

let equal_right_side _ _ = true

let show_free _ = "free"

let pp_free fmt v = Format.fprintf fmt "%s" (show_free v)

let equal_free _ _ = true

type 'a t = {
  file_list : File_list.scanned;
  history : Location_history.t;
}
[@@deriving show, eq]

let make_left ~file_list ~history = { file_list; history }

let make_right ~file_list ~history = { file_list; history }

let as_free { file_list; history } = { file_list; history }

let reload_list file_list t =
  let location = File_list.location file_list and location' = File_list.location t.file_list in
  if Path.equal location location' then Ok { t with file_list } else Error `Not_same

let move_location ~file_list ~timestamp t =
  let location = File_list.location file_list and location' = File_list.location t.file_list in
  if not @@ Path.equal location location' then
    let record = Location_history.Record.make ~location:(File_list.location file_list) ~timestamp in
    Ok { file_list; history = Location_history.add_record record t.history }
  else Error `Same
