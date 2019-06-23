module D = Sxfiler_domain

(* make empty fixture for file tree *)
let empty_list location = D.File_list.make ~location ~items:[] ()
