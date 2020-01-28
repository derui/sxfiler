include struct
  include Stdlib.Int64

  module V = Comparable.Make (struct
    type t = int64

    let compare = Stdlib.Int64.compare
  end)

  include V.Infix

  let ( * ) = mul

  let ( + ) = add

  let ( / ) = div

  let ( - ) = sub

  let ( % ) = rem
end
