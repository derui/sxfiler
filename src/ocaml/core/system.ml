(** This module provides interface and implements for Sys module. *)

(** Interface of system module. *)
module type S = sig
  val getcwd : unit -> string
end

(** Implementation of module signature *)
module Real : S = struct
  let getcwd = Sys.getcwd
end
