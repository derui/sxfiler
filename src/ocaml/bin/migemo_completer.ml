module T = Sxfiler_domain
(** Completer provides simple completion interface via string. *)

include T.Completer

module Core = struct
  type t = {migemo : Migemocaml.Migemo.t}
  (** The type of completer. *)

  let make ~migemo = {migemo}
  let is_some = function Some _ -> true | None -> false

  let read t ~input ~collection =
    let regexp = Migemocaml.Migemo.query ~query:input t.migemo |> Re.Posix.compile_pat in
    List.map (fun s -> (Re.exec_opt regexp s.T.Completion.Item.value, s)) collection
    |> List.filter (fun v -> is_some @@ fst v)
    |> List.map (function
        | None, _ -> failwith "Invalid branch"
        | Some group, v ->
          let start, length = Re.Group.offset group 0 in
          {T.Completion.Candidate.start; length; value = v})
end

let make ~migemo =
  ( module struct
    module Completer = Core

    let this = Core.make ~migemo
  end : Instance )
