(** Define testables for Alcotest for domain objects. *)

module D = Sxfiler_domain

let file_stat = Alcotest.testable D.File_stat.pp D.File_stat.equal
let node = Alcotest.testable D.Node.pp D.Node.equal
let key_map = Alcotest.testable D.Key_map.pp D.Key_map.equal
