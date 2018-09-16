(** This module defines hub to access all service implementation.
    It is useful to use service without export implementation.
*)

module type S = sig
  val completion : unit -> (module Completion.S)
  val configuration : unit -> (module Configuration.S)
  val filer : unit -> (module Filer.S)
  val keymap : unit -> (module Keymap.S)
  val plan : unit -> (module Plan.S)
end
