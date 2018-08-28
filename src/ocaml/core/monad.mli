(** Monad functors *)

include module type of struct
  include Monad_intf
end

module Make (T : Base) : S with type 'a t := 'a T.t
module Make2 (T : Base2) : S2 with type ('a, 'b) t := ('a, 'b) T.t
