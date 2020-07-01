open Sxfiler_core
module D = Sxfiler_domain
module F = Sxfiler_workflow
module T = Sxfiler_translator.Theme
module G = Sxfiler_generated

let store_theme : Path.t -> F.Common_step.Theme.Store_theme.t =
 fun dir theme ->
  let module NE = D.Common.Not_empty_string in
  let module S = F.Common_step.Theme.Store_theme in
  let dir' = Path.to_string dir in
  let theme_name = NE.value theme.D.Theme.name in
  let theme_file = theme_name |> Path.join dir |> Path.to_string in

  if not & Sys.file_exists dir' then Lwt.return_error S.(Unknown_error ("Directory not found " ^ dir'))
  else if not & Sys.is_directory dir' then Lwt.return_error S.(Unknown_error (dir' ^ " is not directory"))
  else if Sys.file_exists theme_file then Lwt.return_error S.(Duplicate_name theme_name)
  else
    let json = T.of_domain theme |> G.Theme.Theme.to_json in
    let%lwt () =
      Lwt_io.with_file ~mode:Lwt_io.Output theme_file (fun oc -> Lwt_io.write_line oc & Yojson.Safe.to_string json)
    in
    Lwt.return_ok ()

let remove_theme : Path.t -> F.Common_step.Theme.Remove_theme.t =
 fun dir theme ->
  let module NE = D.Common.Not_empty_string in
  let module S = F.Common_step.Theme.Remove_theme in
  let theme_name = NE.value theme.D.Theme.name in
  let theme_file = theme_name |> Path.join dir |> Path.to_string in

  let open Lwt.Infix in
  Lwt.catch
    (fun () -> Lwt_unix.unlink theme_file >>= Lwt.return_ok)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_error S.(Not_found_theme theme_name)
      | Unix.Unix_error (e, _, _)           -> Lwt.return_error S.(Unknown_error (Unix.error_message e))
      | _                                   -> Lwt.return_error S.(Unknown_error "Unhandled error"))

let list_theme : Path.t -> F.Common_step.Theme.List_theme.t =
 fun dir () ->
  let module NE = D.Common.Not_empty_string in
  let dir' = Path.to_string dir in

  Lwt.catch
    (fun () ->
      let themes = Sys.readdir dir' |> Array.map (Path.join dir %> Path.to_string) in
      let themes =
        Array.fold_left
          (fun themes theme ->
            try
              let proto = Yojson.Safe.from_file theme |> G.Theme.Theme.of_json in
              match proto with
              | Error _  -> themes
              | Ok proto -> ( match T.to_domain proto with Error _ -> themes | Ok theme -> theme :: themes )
            with _ -> themes)
          [] themes
      in
      Lwt.return themes)
    (fun _ -> Lwt.return [])
