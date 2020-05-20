(** Define testables for Alcotest for domain objects. *)

module D = Sxfiler_domain

let file_stat = Alcotest.testable D.File_stat.pp D.File_stat.equal

let file_item = Alcotest.testable D.File_item.pp D.File_item.equal

let file_list_scanned = Alcotest.testable D.File_list.pp_scanned D.File_list.equal_scanned

let bookmark_name = Alcotest.testable D.Bookmarks.Name.pp D.Bookmarks.Name.equal

let bookmark_item = Alcotest.testable D.Bookmarks.Item.pp D.Bookmarks.Item.equal

let keymap = Alcotest.testable D.Keymap.pp D.Keymap.equal

let keymap_binding = Alcotest.testable D.Keymap.Binding.pp D.Keymap.Binding.equal

let keymap_action = Alcotest.testable D.Keymap.Action.pp D.Keymap.Action.equal

module Interaction = struct
  let filer_copy_selected =
    Alcotest.testable D.Interaction.Filer_copy_selected.pp D.Interaction.Filer_copy_selected.equal

  let filer_move_selected =
    Alcotest.testable D.Interaction.Filer_move_selected.pp D.Interaction.Filer_move_selected.equal

  let filer_delete_selected =
    Alcotest.testable D.Interaction.Filer_delete_selected.pp D.Interaction.Filer_delete_selected.equal
end

module Completer = struct
  let item = Alcotest.testable D.Completer.Item.pp D.Completer.Item.equal

  let canditate = Alcotest.testable D.Completer.Candidate.pp D.Completer.Candidate.equal
end

module History = struct
  let t = Alcotest.testable D.Location_history.pp D.Location_history.equal

  let record = Alcotest.testable D.Location_history.Record.pp D.Location_history.Record.equal
end
