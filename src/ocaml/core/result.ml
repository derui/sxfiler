include Stdlib.Result

include Monad.Make2 (struct
  type ('a, 'b) t = ('a, 'b) result

  let bind m ~f = Stdlib.Result.bind m f
  let fmap = `Use_bind_to_define
  let return v = Ok v
end)

include Infix
