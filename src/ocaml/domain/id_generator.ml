module type S = sig
  type id

  val generate : unit -> id
  (** [generate ()] generate an identifier *)
end
