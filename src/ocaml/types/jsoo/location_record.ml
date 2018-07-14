include Sxfiler_types.Location_record

class type js = object
  method location: Js.js_string Js.t Js.readonly_prop
  method timestamp: Js.js_string Js.t Js.readonly_prop
end

let of_js: js Js.t -> t = fun js ->
  {
    location = Js.to_string js##.location;
    timestamp = Int64.of_string @@ Js.to_string js##.timestamp;
  }
