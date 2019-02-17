module D = Sxfiler_domain

(* make empty fixture for file tree *)
let empty_tree location = D.File_tree.make ~location ~nodes:[]
