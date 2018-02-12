type t = {
  key_map: Key_map.t;
}

class type js = object
  method keyMap: Key_map.t Js.readonly_prop
end

let empty = {
  key_map = Key_map.empty;
}

let to_js t = object%js
  val keyMap = t.key_map
end

let of_js js = {
  key_map = js##.keyMap;
}
