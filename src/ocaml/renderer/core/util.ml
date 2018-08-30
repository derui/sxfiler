let special_key_mapping = function " " -> "Space" | _ as key -> key

let keyboard_event_to_key v =
  let module K = Sxfiler_kbd in
  Logs.app (fun m -> m "key: %s" (Js.to_string v##.key)) ;
  K.to_keyseq
  @@ K.make
    ~ctrl:(Js.to_bool v##.ctrlKey)
    ~meta:(Js.to_bool v##.altKey)
    (special_key_mapping @@ Js.to_string v##.key)

let find_item_index ?(equal = ( = )) ~v array =
  let rec find ary v ind =
    if Array.length ary <= ind then 0 else if equal v ary.(ind) then ind else find ary v (succ ind)
  in
  find array v 0

(* More easy handling for {!Js.Optdef}. *)
module Optdef = struct
  let ( >>= ) a f = Js.Optdef.bind a f
end
