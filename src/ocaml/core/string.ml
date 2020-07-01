include Stdlib.String

let split_by_len ~len str =
  let len = if len <= 0 then 1 else len in
  let rec split str accum =
    if length str = 0 then List.rev accum
    else if length str <= len then List.rev (str :: accum)
    else
      let head = sub str 0 len in
      let rest = sub str len (length str - len) in
      split rest (head :: accum)
  in
  split str []
