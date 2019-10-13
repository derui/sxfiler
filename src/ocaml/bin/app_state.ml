(** Persist current status of the application *)

module S = Sxfiler_domain
module T = Sxfiler_server_translator

type filer_stat =
  { id : string
  ; name : string
  ; location : string
  ; history : T.Location_history.t }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

type t =
  { filers : filer_stat list
  ; bookmarks : T.Bookmark.t list }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let empty = {filers = []; bookmarks = []}

let put_bookmarks bookmarks stat =
  let bookmarks = List.map T.Bookmark.of_domain bookmarks in
  {stat with bookmarks}

let restore_bookmarks stat = List.map T.Bookmark.to_domain stat.bookmarks

(** [add_filer_stat filer stat] add the filer to stat *)
let add_filer_stat (filer : S.Filer.t) stat =
  let filer' = T.Filer.of_domain filer in
  let value =
    { id = filer'.id
    ; name = filer'.name
    ; location = filer'.file_list.location
    ; history = filer'.history }
  in
  {stat with filers = value :: stat.filers}

(** [add_filer_stat filer stat] add the filer to stat *)
let restore_filer_stats ~(scanner : (module S.Location_scanner_service.S)) stat =
  let module Scan = (val scanner) in
  let convert_filer value =
    let v' =
      { T.Filer.id = value.id
      ; name = value.name
      ; history = value.history
      ; sort_order = T.Types.Sort_type.Name
      ; marked_items = []
      ; file_list = {location = value.location; items = []} }
    in
    T.Filer.to_domain v'
  in
  let filers = List.map convert_filer stat.filers in
  Lwt_list.map_p
    (fun v ->
      let%lwt file_list = Scan.scan v.S.Filer.file_list.location in
      S.Filer.update_list v ~file_list |> Lwt.return)
    filers
