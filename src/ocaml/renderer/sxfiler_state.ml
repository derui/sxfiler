module T = Sxfiler_common.Std.Types
module FFI = Sxfiler_common.Std.Ffi
module Thread = Lwt

(* All state of this application *)
type t = {
  file_list: T.File_stat.t list;
  waiting: bool;
}

class type _js = object
  method file_list: T.File_stat.js Js.t Js.js_array Js.t Js.readonly_prop
  method waiting: bool Js.t Js.readonly_prop
end
type js = _js Js.t

type message = Sxfiler_message.t

type command = message Thread.t

let equal = ( = )
let update t = function
  | _ -> failwith "not implemented"

let empty () = {
  file_list = [];
  waiting = false;
}

let to_js t = object%js
  val file_list = List.map T.File_stat.to_js t.file_list
                  |> Array.of_list
                  |> Js.array
  val waiting = Js.bool t.waiting
end
