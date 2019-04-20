module D = Sxfiler_domain

(* make fixture for filer *)
let fixture ?(history = D.Location_history.(make ())) ~file_tree ~sort_order id =
  let module Factory = D.Filer.Factory.Make in
  Factory.create ~name:id ~file_tree ~history ~sort_order
