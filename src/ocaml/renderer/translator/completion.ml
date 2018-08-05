module D = Sxfiler_completion.Domain

module Item = struct
  open D.Item
  class type js = object
    method id: Js.js_string Js.t Js.readonly_prop
    method value: Js.js_string Js.t Js.readonly_prop
  end

  let to_js: t -> js Js.t = fun t -> object%js
    val id = Js.string t.id
    val value = Js.string t.value
  end

  let of_js: js Js.t -> t = fun js -> {
      id = Js.to_string js##.id;
      value = Js.to_string js##.value;
    }
end

module Candidate = struct
  open D.Candidate
  class type js = object
    method start: int Js.readonly_prop
    method length: int Js.readonly_prop
    method value: Item.js Js.t Js.readonly_prop
  end

  let to_js: t -> js Js.t = fun t -> object%js
    val start = t.start
    val length = t.length
    val value = Item.to_js t.value
  end

  let of_js: js Js.t -> t = fun js -> {
      start = js##.start;
      length = js##.length;
      value = Item.of_js js##.value;
    }
end
