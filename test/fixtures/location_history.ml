module D = Sxfiler_domain

(* make fixture for node *)
let fixture records =
  List.fold_left
    (fun data record -> D.Location_history.add_record ~record data)
    (D.Location_history.make ()) records
