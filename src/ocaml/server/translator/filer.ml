module D = Sxfiler_domain.Filer

type t =
  { id : string
  ; file_tree : File_tree.t
  ; history : Location_history.t
  ; selected_nodes : string list
  ; sort_order : Types.Sort_type.t }
[@@deriving yojson]

let of_domain t =
  { id = t.D.id
  ; file_tree = File_tree.of_domain t.file_tree
  ; sort_order = Types.Sort_type.of_domain t.sort_order
  ; selected_nodes = t.selected_nodes
  ; history = Location_history.of_domain t.history }

let to_domain t =
  D.make ~id:t.id ~file_tree:(File_tree.to_domain t.file_tree)
    ~sort_order:(Types.Sort_type.to_domain t.sort_order)
    ~selected_nodes:t.selected_nodes
    ~history:(Location_history.to_domain t.history)
    ()