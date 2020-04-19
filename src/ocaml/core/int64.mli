(** This module provides more functionally for {int64} *)
include sig
  include module type of Stdlib.Int64

  val ( = ) : int64 -> int64 -> bool

  val ( < ) : int64 -> int64 -> bool

  val ( > ) : int64 -> int64 -> bool

  val ( <= ) : int64 -> int64 -> bool

  val ( >= ) : int64 -> int64 -> bool

  val ( <> ) : int64 -> int64 -> bool

  val ( * ) : int64 -> int64 -> int64

  val ( + ) : int64 -> int64 -> int64

  val ( / ) : int64 -> int64 -> int64

  val ( - ) : int64 -> int64 -> int64

  val ( % ) : int64 -> int64 -> int64
end
