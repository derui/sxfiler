open Sxfiler_core
module D = Sxfiler_domain
module F = Sxfiler_workflow

let resolve_keymap (module M : Statable.S with type state = D.Keymap.t) : F.Common_step.Keymap.resolve_keymap =
 fun () -> M.get ()

let store_keymap (module M : Statable.S with type state = D.Keymap.t) : F.Common_step.Keymap.store_keymap =
 fun keymap -> M.update keymap

let load_keymap path =
  let keymap = Keymap_file.load path in
  keymap |> Result.map_error (fun e -> `Illegal_keymap (Keymap_file.show_error e)) |> Lwt.return
