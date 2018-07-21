(** This module defines behavior for initialize stores. *)
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core

type message = C.Message.t
type t = {
  scanner_repo: (module C.Repository_intf.Scanner_instance);
  keybindings_repo: (module C.Repository_intf.Keybindings_instance);
  configuration_repo: (module C.Repository_intf.Configuration_instance);
}

type param = (module C.Store_intf.Instance with type message = C.Message.t)
type result = unit

let make locator =
  let module L = (val locator : C.Locator_intf.S) in
  {
    scanner_repo = (module L.Repository.Scanner);
    keybindings_repo = (module L.Repository.Keybindings);
    configuration_repo = (module L.Repository.Configuration);
  }

let execute t _ store =
  let module Store = (val store : C.Store_intf.Instance with type message = C.Message.t) in
  let module Rs = (val t.scanner_repo) in
  Rs.(Repo.on_change instance @@ fun v ->
      Store.(Store.dispatch instance C.Message.(Update_scanner v)));

  let module Rk = (val t.keybindings_repo) in
  Rk.(Repo.on_change instance @@ fun v ->
      Store.(Store.dispatch instance C.Message.(Update_keymap v)));

  let module Rc = (val t.configuration_repo) in
  Rc.(Repo.on_change instance @@ fun v ->
      Store.(Store.dispatch instance C.Message.(Update_configuration v)));
