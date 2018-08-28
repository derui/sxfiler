open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let data =
  T.File_stat.
    { mode = Int32.(to_string @@ of_int 0o775)
    ; uid = 1000
    ; gid = 1000
    ; atime = Int64.to_string @@ Int64.of_int 100
    ; mtime = Int64.to_string @@ Int64.of_int 1000
    ; ctime = Int64.to_string @@ Int64.of_int 10000
    ; size = Int64.to_string @@ Int64.max_int
    ; is_directory = true
    ; is_file = false
    ; is_symlink = true }


let () =
  "File stat translator"
  >::: [ ( "should be able to convert between JavaScript and OCaml"
           >:: fun () -> assert_ok (data = Tr.File_stat.(of_js @@ to_js data)) ) ]
