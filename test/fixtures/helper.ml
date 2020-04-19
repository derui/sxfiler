let chars =
  [|
    'a';
    'b';
    'c';
    'd';
    'e';
    'f';
    'g';
    'h';
    'i';
    'j';
    'k';
    'l';
    'm';
    'n';
    'o';
    'p';
    'q';
    'r';
    's';
    't';
    'u';
    'v';
    'w';
    'x';
    'y';
    'z';
  |]

let chars_size = Array.length chars

(* get random string in [chars] *)
let random_string state len =
  let rec loop rest_len accum =
    if rest_len <= 0 then List.rev accum
    else
      let char = chars.(Random.State.int state chars_size) in
      loop (pred rest_len) (char :: accum)
  in
  loop len [] |> List.to_seq |> String.of_seq
