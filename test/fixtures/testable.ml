(** Define testables for Alcotest for domain objects. *)

module D = Sxfiler_domain

let file_stat = Alcotest.testable D.File_stat.pp D.File_stat.equal
let file_item = Alcotest.testable D.File_item.pp D.File_item.equal
let key_map = Alcotest.testable D.Key_map.pp D.Key_map.equal
