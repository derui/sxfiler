module A = App_state

module Key = struct
  type key =
    | Filer_location
    | Filer_histories
    | Bookmarks
  [@@deriving show]

  type 'a t = key * 'a A.typ

  let filer_location : (string * string) t = (Filer_location, A.Pair (A.String, A.String))

  let filer_histories = (Filer_histories, A.Pair (A.List A.(A.String %* A.String), A.List A.(A.String %* A.String)))

  let bookmarks = (Bookmarks, A.List A.(String %* String))
end

let read_by ~key:(key, typ) state = A.read_state ~typ ~key:(Key.show_key key) state

let write_by : type a. key:a Key.t -> value:a -> A.app_state -> unit =
 fun ~key:(key, typ) ~value state -> A.write_state ~typ ~key:(Key.show_key key) ~value state
