module Key_map = Common_key_map
type key_maps = {
  file_list: Key_map.t;
}

type t = {
  key_maps: key_maps;
}

class type js = object
  method keyMapFileList: Key_map.t Js.readonly_prop
end

let empty = {
  key_maps = {
    file_list = Key_map.empty;
  }
}

let to_js t = object%js
  val keyMapFileList = t.key_maps.file_list
end

let of_js js = {
  key_maps = {
    file_list = js##.keyMapFileList;
  }
}
