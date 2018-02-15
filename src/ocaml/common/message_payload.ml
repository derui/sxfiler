(** This module provides payloads for message *)
module T = Sxfiler_types

module Request_copy_file = struct
  type same_name_behavior =
    | Overwrite
    | Rename of Js.js_string Js.t
    | Noop

  type t = {
    src: T.File_stat.js Js.t;
    dest_dir: Js.js_string Js.t;
    same_name_behavior: same_name_behavior;
  }
end
