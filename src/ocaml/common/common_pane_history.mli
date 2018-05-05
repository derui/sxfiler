
module T = Common_types
module Record :
sig
  type t = {
    directory : string;
    focused_item : T.file_id option;
    timestamp : int64;
  }

  class type js = object
    method directory : Js.js_string Js.t Js.readonly_prop
    method focusedItem : Js.js_string Js.t Js.opt Js.readonly_prop
    method timestamp : float Js.readonly_prop
  end

  val make :
    timestamp:int64 ->
    directory:string -> ?focused_item:T.file_id -> unit -> t

  val of_js : js Js.t -> t
end

type t

class type js = object
  method records : Record.js Js.t Js.js_array Js.t Js.readonly_prop
  method maxRecords : int Js.readonly_prop
end

(** Make the history. *)
val empty : unit -> t

(** Convert Js to OCaml *)
val of_js : js Js.t -> t
