module D = Sxfiler_domain.Filer

type t =
  { id : string
  ; file_tree : File_tree.t [@key "fileTree"]
  ; history : Location_history.t
  ; marked_nodes : string list [@key "markedNodes"]
  ; sort_order : Types.Sort_type.t [@key "sortOrder"] }
[@@deriving show, yojson]

let of_domain t =
  { id = t.D.id
  ; file_tree = File_tree.of_domain t.file_tree
  ; sort_order = Types.Sort_type.of_domain t.sort_order
  ; marked_nodes = D.Node_id_set.to_seq t.marked_nodes |> List.of_seq
  ; history = Location_history.of_domain t.history }

let to_domain t =
  D.make ~id:t.id ~file_tree:(File_tree.to_domain t.file_tree)
    ~sort_order:(Types.Sort_type.to_domain t.sort_order)
    ~marked_nodes:(D.Node_id_set.of_list t.marked_nodes)
    ~history:(Location_history.to_domain t.history)
