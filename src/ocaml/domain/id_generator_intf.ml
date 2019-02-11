module type Gen_random = sig
  type id

  val generate : unit -> id
end
