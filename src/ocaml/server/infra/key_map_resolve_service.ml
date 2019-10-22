open Sxfiler_core
module D = Sxfiler_domain

module type Location = sig
  val path : Path.t
end

module Make (L : Location) : D.Key_map_resolve_service.S = struct
  let resolve () =
    let path = Path.to_string L.path in
    let keymap = Yojson.Safe.from_file path in
    let module Y = Sxfiler_server_translator.Key_map in
    match Y.of_json keymap with
    | Error err ->
        Logs.warn (fun m ->
            m "Error occurred: %s" @@ Protocol_conv_json.Json.error_to_string_hum err) ;
        Lwt.fail_with "Invalid key map"
    | Ok v -> Lwt.return @@ Y.to_domain v
end
