module M = Sxfiler_message
module T = Sxfiler_types
module FFI = Sxfiler_ffi
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

type message = M.t

type command = message Thread.t

let equal = ( = )
let update t = function
  | M.FINISH_FILES_IN_DIRECTORY (_, _, list) -> ({t with file_list = Array.to_list list}, None)
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

let of_js t =
  let file_list = Js.to_array t##.file_list in
  let file_list = Array.to_list file_list
                  |> List.map T.File_stat.of_js
  in
  {file_list;
   waiting = Js.to_bool t##.waiting;
  }