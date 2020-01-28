open Sxfiler_core

module T = Sxfiler_domain
(** Completer provides simple completion interface via string. *)

module Core = struct
  type t = { migemo : Migemocaml.Migemo.t }
  (** The type of completer. *)

  let make ~migemo = { migemo }

  let read ~input ~collection t =
    let regexp = Migemocaml.Migemo.query ~query:input t.migemo |> Re.Posix.compile_pat in
    List.map
      (fun s ->
        let open Option in
        let+ group = Re.exec_opt regexp s.T.Completer.Item.value in
        (group, s))
      collection
    |> List.map Option.to_list |> List.concat
    |> List.map (function group, v ->
           let start, length = Re.Group.offset group 0 in
           T.Completer.Candidate.make ~start ~length:(length - start) ~value:v)
end

let make ~migemo =
  ( module struct
    module Completer = Core

    let this = Core.make ~migemo
  end : T.Completer.Instance )
