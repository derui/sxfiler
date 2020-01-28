include Stdlib.Option

include Monad.Make (struct
  type 'a t = 'a option

  let bind m ~f = Stdlib.Option.bind m f

  let fmap = `Use_bind_to_define

  let return v = Some v
end)

include Infix
