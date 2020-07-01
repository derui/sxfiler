let ( = ) = Int.equal

let (( == )[@deprecated "Use phys_equal instead"]) = Stdlib.( == )

module String = String
module Comparable = Comparable
module Error = Error
module File = File
module Fun = Fun
module Int64 = Int64
module Monad = Monad
module Option = Option
module Path = Path
module Result = Result
module System = System
module Time = Time

(* export infix operator for FP technique *)
include Fun.Infix
