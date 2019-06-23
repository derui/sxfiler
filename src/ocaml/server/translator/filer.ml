open Sxfiler_core
module D = Sxfiler_domain.Filer

type t =
  { id : string
  ; name : string
  ; file_list : File_list.t [@key "fileList"]
  ; history : Location_history.t
  ; marked_items : string list [@key "markedItems"]
  ; sort_order : Types.Sort_type.t [@key "sortOrder"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let of_domain t =
  { id = Uuidm.to_string t.D.id
  ; name = t.name
  ; file_list = File_list.of_domain t.file_list
  ; sort_order = Types.Sort_type.of_domain t.sort_order
  ; marked_items = D.Marked_item_set.to_seq t.marked_items |> List.of_seq
  ; history = Location_history.of_domain t.history }

let to_domain t =
  let id = Uuidm.of_string t.id |> Option.get_exn in
  D.make ~id ~name:t.name ~file_list:(File_list.to_domain t.file_list)
    ~sort_order:(Types.Sort_type.to_domain t.sort_order)
    ~marked_items:(D.Marked_item_set.of_list t.marked_items)
    ~history:(Location_history.to_domain t.history)
