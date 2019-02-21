module D = Sxfiler_domain

(* make fixture for filer *)
let fixture ?(history = D.Location_history.(make ())) ~file_tree ~sort_order id =
  let module Factory = D.Filer.Factory.Make (struct
      type id = string

      let generate () = id
    end) in
  Factory.create ~file_tree ~history ~sort_order
