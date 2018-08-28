(** Completer provides simple completion interface via string. *)
module T = Sxfiler_completion.Domain

include Sxfiler_completion.Completer_intf

module Core = struct
  (** The type of completer. *)
  type t = {migemo : Migemocaml.Migemo.t}

  let make ~migemo = {migemo}
  let is_some = function Some _ -> true | None -> false

  let read (type v) t ~input ~collection ~(stringify : (module Type with type t = v)) =
    let module S = (val stringify : Type with type t = v) in
    let regexp = Migemocaml.Migemo.query ~query:input t.migemo |> Re.Posix.compile_pat in
    let collection = List.map (fun v -> (S.to_string v, v)) collection in
    List.map (fun s -> (Re.exec_opt regexp @@ fst s, snd s)) collection
    |> List.filter (fun v -> is_some @@ fst v)
    |> List.map (function
        | None, _ ->
          failwith "Invalid branch"
        | Some group, v ->
          let start, length = Re.Group.offset group 0 in
          {T.Candidate.start; length; value = v} )
end

let make ~migemo =
  ( module struct
    module Completer = Core

    let this = Core.make ~migemo
  end
  : Instance )
