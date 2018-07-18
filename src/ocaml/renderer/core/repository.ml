module T = Sxfiler_types

include Repository_intf

module Scanner : Scanner = struct
  type t = {
    mutable subscribers: (T.Scanner.t -> unit) list;
    table: T.Scanner.t Jstable.t;
  }

  let make () = {
    subscribers = [];
    table = Jstable.create ();
  }

  let get t name =
    let open Sxfiler_core.Option in
    get_exn @@ Js.Optdef.to_option @@ Jstable.find t.table Js.(string name)

  let store t scanner =
    let name = scanner.T.Scanner.location in
    Jstable.add t.table Js.(string name) scanner;
    List.iter (fun f -> f scanner) t.subscribers

  let on_change t f =
    t.subscribers <- f :: t.subscribers

end

module Keybindings : Keybindings = struct
  type t = {
    mutable subscribers: (Key_map.t -> unit) list;
    mutable key_map: Key_map.t;
  }

  let make () = {
    subscribers = [];
    key_map = Key_map.empty;
  }

  let get {key_map;_} = key_map

  let store t key_map =
    t.key_map <- key_map;
    List.iter (fun f -> f key_map) t.subscribers

  let on_change t f =
    t.subscribers <- f :: t.subscribers

end

module Configuration : Configuration = struct
  type t = {
    mutable subscribers: (T.Configuration.t -> unit) list;
    mutable configuration: T.Configuration.t;
  }

  let make () = {
    subscribers = [];
    configuration = T.Configuration.default;
  }

  let get {configuration;_} = configuration

  let store t v =
    t.configuration <- v;
    List.iter (fun f -> f v) t.subscribers

  let on_change t f =
    t.subscribers <- f :: t.subscribers

end
