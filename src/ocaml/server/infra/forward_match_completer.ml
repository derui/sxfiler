open Sxfiler_core

module T = Sxfiler_domain
(** Completer provides simple completion interface via string. *)

include T.Completer

module Core = struct
  type t = unit

  let make () = ()
  let is_some = function Some _ -> true | None -> false

  let read () ~input ~collection =
    let regexp = Re.Posix.compile_pat @@ "^" ^ input in
    List.map (fun s -> (Re.exec_opt regexp s.T.Completion.Item.value, s)) collection
    |> (List.filter @@ Fun.(fst %> is_some))
    |> List.map (function
         | None, _ -> failwith "Invalid branch"
         | Some group, v ->
             let start, length = Re.Group.offset group 0 in
             {T.Completion.Candidate.start; length = length - start; value = v})
end

let make () =
  ( module struct
    module Completer = Core

    let this = Core.make ()
  end : Instance )
