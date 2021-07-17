let mk_temp_dir ?mode ?dir pat =
  let dir = match dir with None -> Filename.get_temp_dir_name () | Some v -> v
  and mode = match mode with None -> 0o755 | Some v -> v in
  let make_pattern () =
    let rand = Random.int 0x1000000 in
    Printf.sprintf "%s%06x" pat rand
  in
  let rec loop count =
    let pat = make_pattern () in
    let temp_dir = Filename.concat dir pat in
    match Unix.mkdir temp_dir mode with
    | () -> temp_dir
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> loop (pred count)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop count
    | exception Unix.Unix_error (e, _, _) -> failwith (Printf.sprintf "mk_temp_dir: %s" (Unix.error_message e))
  in
  loop 1000

let remove ?(recursive = true) path =
  let open Fun.Infix in
  if not & Sys.file_exists path then ()
  else if not recursive then Sys.remove path
  else
    let rec remove_all_structure paths =
      match paths with
      | []           -> ()
      | path :: rest ->
          if Sys.is_directory path then (
            let entries = Sys.readdir path |> Array.map (Filename.concat path) |> Array.to_list in
            remove_all_structure entries;
            Unix.rmdir path)
          else Sys.remove path;
          remove_all_structure rest
    in
    remove_all_structure [ path ]
