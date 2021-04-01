open Sxfiler_core
module D = Sxfiler_domain
module F = Sxfiler_workflow

module type State = Statable.S with type state = Sxfiler_domain.Keymap.t

module Instance (S : State) = struct
  let resolve_keymap () = S.get ()

  let store_keymap keymap = S.update keymap

  let load_keymap path =
    let keymap = Keymap_file.load path in
    keymap |> Result.map_error (fun e -> `Illegal_keymap (Keymap_file.show_error e)) |> Lwt.return
end
