open Sxfiler_core

module Operation = struct
  type t =
    | Append
    | Delete
    | Remained

  let to_int = function Append -> 0 | Delete -> 1 | Remained -> 2
  let of_int = function 0 -> Some Append | 1 -> Some Delete | 2 -> Some Remained | _ -> None
end

type plan =
  { operation : Operation.t
  ; node : Node.t }

type env =
  { from : Filer.id
  ; _to : Filer.id }

type simulated =
  { from : plan list
  ; _to : plan list }

type id = Uuidm.t

type t =
  { id : id
  ; env : env
  ; nodes : Node.t list
  ; simulated : simulated option }

let make ~id ~env ~nodes = {id; env; nodes; simulated = None}
let planning t ~simulated = {t with simulated = Some simulated}
let already_simulated t = Option.is_some t.simulated

module type Repository = sig
  val resolve : id -> t option Lwt.t
  val store : t -> unit Lwt.t
end
