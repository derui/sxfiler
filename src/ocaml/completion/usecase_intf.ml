(** signature of use case to setup completion*)
module type Setup = sig
  type input = {source : Domain.collection}
  type output = unit Lwt.t

  val execute : input -> output
end

(** signature of use case to read current completion *)
module type Read = sig
  type input = {input : string}
  type output = Domain.result Lwt.t

  val execute : input -> output
end
