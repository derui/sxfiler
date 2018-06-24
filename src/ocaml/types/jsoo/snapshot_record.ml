include Sxfiler_types.Snapshot_record

class type js = object
  method directory: Js.js_string Js.t Js.readonly_prop
  method timestamp: Js.js_string Js.t Js.readonly_prop
end

let of_js: js Js.t -> t = fun js ->
  {
    directory = Js.to_string js##.directory;
    timestamp = Int64.of_string @@ Js.to_string js##.timestamp;
  }
