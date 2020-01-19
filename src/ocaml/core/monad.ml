include Monad_intf

module Make (T : Base) : S with type 'a t := 'a T.t = struct
  let bind = T.bind
  let return = T.return
  let lift f v = return (f v)

  let fmap =
    match T.fmap with `Use_bind_to_define -> fun m ~f -> bind m ~f:(lift f) | `Use_original f -> f

  module Infix = struct
    let ( >>= ) m f = bind m ~f
    let ( >|= ) m f = fmap m ~f
    let ( let* ) m f = bind m ~f
    let ( let+ ) m f = fmap m ~f
  end
end

module Make2 (T : Base2) : S2 with type ('a, 'b) t := ('a, 'b) T.t = struct
  let bind = T.bind
  let return = T.return
  let lift f v = return @@ f v

  let fmap =
    match T.fmap with `Use_bind_to_define -> fun m ~f -> bind m ~f:(lift f) | `Use_original f -> f

  module Infix = struct
    let ( >>= ) m f = bind m ~f
    let ( >|= ) m f = fmap m ~f
    let ( let* ) m f = bind m ~f
    let ( let+ ) m f = fmap m ~f
  end
end
