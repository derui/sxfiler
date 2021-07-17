open Sxfiler_core

module T = Sxfiler_domain
(** Completer provides simple completion interface via string. *)

module Core = struct
  type t = unit

  let read ~input ~collection () =
    let regexp = Re.Posix.compile_pat & ("^" ^ input) in
    collection
    |> List.map (fun s ->
           let open Option in
           let+ group = Re.exec_opt regexp s.T.Completer.Item.value in
           (group, s))
    |> List.map Option.to_list |> List.concat
    |> List.map (fun (group, v) ->
           let start, length = Re.Group.offset group 0 in
           T.Completer.Candidate.make ~start ~length:(length - start) ~value:v)
end

let make () =
  (module struct
    module Completer = Core

    let this = ()
  end : T.Completer.Instance)
