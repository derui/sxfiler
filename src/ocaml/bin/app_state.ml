open Sxfiler_core
(** Persist current status of the application *)

module D = Sxfiler_domain
module F = Sxfiler_workflow
module T = Sxfiler_translator
module G = Sxfiler_generated
module Tr = Sxfiler_translator

type t = {
  filer : G.Filer.Filer.t option;
  bookmarks : G.Bookmark.BookmarkList.t option;
}
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let empty = { filer = None; bookmarks = None }

let put_bookmarks bookmarks stat = { stat with bookmarks = Some bookmarks }

let restore_bookmarks { bookmarks; _ } = bookmarks |> Option.map Tr.Bookmarks.to_domain

(** [add_filer_stat filer stat] add the filer to stat *)
let add_filer_stat (filer : D.Filer.t) stat =
  let filer' = Tr.Filer.of_domain filer in
  { stat with filer = Some filer' }

(** [add_filer_stat filer stat] add the filer to stat *)
let restore_filer_stats ~(initialize : F.Filer.Initialize.work_flow) stat =
  let open Option.Infix in
  let file_lists =
    let* filer = stat.filer in
    let* left_location =
      filer.left_file_window >>= fun v ->
      v.G.Filer.FileWindow.file_list >|= fun v -> v.location |> Path.of_string
    in
    let* right_location =
      filer.right_file_window >>= fun v ->
      v.G.Filer.FileWindow.file_list >|= fun v -> v.location |> Path.of_string
    in
    let left_history =
      filer.left_file_window >>= fun v ->
      v.G.Filer.FileWindow.history >>= fun v -> Tr.Location_history.to_domain v |> Result.to_option
    in
    let right_history =
      filer.right_file_window >>= fun v ->
      v.G.Filer.FileWindow.history >>= fun v -> Tr.Location_history.to_domain v |> Result.to_option
    in
    Some (left_location, right_location, left_history, right_history)
  in
  match file_lists with
  | Some (Ok left_location, Ok right_location, left_history, right_history) ->
      let input = { F.Filer.Initialize.left_location; right_location; left_history; right_history } in
      let open Lwt.Infix in
      initialize input >|= List.map (fun v -> F.Filer v)
  | Some _ | None -> Lwt.return []
