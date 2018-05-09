module Key_map = Common_key_map

module Key_maps = struct
  type t = {
    file_list: Key_map.t;
  }

  class type js = object
    method fileList: Key_map.js Js.t Js.readonly_prop
  end

  let empty = {
    file_list = Key_map.empty;
  }

  let of_js : js Js.t -> t = fun js -> {
      file_list = Key_map.of_js js##.fileList;
    }
end

type t = {
  key_maps: Key_maps.t;
}

class type js = object
  method keyMaps: Key_maps.js Js.t Js.readonly_prop
end

let empty = {
  key_maps = Key_maps.empty;
}

let of_js js = {
  key_maps = {
    file_list = js##.keyMapFileList;
  }
}
