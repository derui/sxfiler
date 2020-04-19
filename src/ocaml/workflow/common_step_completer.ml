module D = Sxfiler_domain

type provide_collection = unit -> D.Completer.collection Lwt.t
(** collection provider *)

type update_collection = D.Completer.collection -> unit Lwt.t
(** collection updater *)

type read = provide_collection -> (module D.Completer.Instance) -> string -> D.Completer.candidates Lwt.t
(** signature to get candidates from collection with input *)

let read : read =
 fun provide_collection (module I : D.Completer.Instance) input ->
  let%lwt collection = provide_collection () in
  I.Completer.read ~input ~collection I.this |> Lwt.return
