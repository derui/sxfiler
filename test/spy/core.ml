module Wrap = struct
  type 'a t = { mutable called_args : 'a list }

  let wrap f =
    let t = { called_args = [] } in
    let wrapped_f arg =
      t.called_args <- arg :: t.called_args;
      f arg
    in
    (t, wrapped_f)

  let called_args { called_args } = List.rev called_args
  let called_count { called_args } = List.length called_args
end

module Wrap2 = struct
  type ('a, 'b) t = { mutable called_args : ('a * 'b) list }

  let wrap f =
    let t = { called_args = [] } in
    let wrapped_f a1 a2 =
      t.called_args <- (a1, a2) :: t.called_args;
      f a1 a2
    in
    (t, wrapped_f)

  let called_args { called_args } = List.rev called_args
  let called_count { called_args } = List.length called_args
end

let wrap = Wrap.wrap
let wrap2 = Wrap2.wrap
