include (struct include Sxfiler_domain.File_stat end)

(* In ppx_deriving_yojson, int32 and int64 are converted as JavaScript's number that has range double.
   The double can not handle int64, so use string as int64 and int32 representation.
*)

class type js = object
  method mode: Js.js_string Js.t Js.readonly_prop
  method uid: int Js.readonly_prop
  method gid: int Js.readonly_prop
  method atime: Js.js_string Js.t Js.readonly_prop
  method ctime: Js.js_string Js.t Js.readonly_prop
  method mtime: Js.js_string Js.t Js.readonly_prop
  method size: Js.js_string Js.t Js.readonly_prop
  method isDirectory: bool Js.t Js.readonly_prop
  method isFile: bool Js.t Js.readonly_prop
  method isSymlink: bool Js.t Js.readonly_prop
end

let of_js : js Js.t -> t = fun js -> {
    mode = Int32.of_string @@ Js.to_string js##.mode;
    uid = js##.uid;
    gid = js##.gid;
    atime = Int64.of_string @@ Js.to_string js##.atime;
    ctime = Int64.of_string @@ Js.to_string js##.ctime;
    mtime = Int64.of_string @@ Js.to_string js##.mtime;
    size = Int64.of_string @@ Js.to_string js##.size;
    is_directory = Js.to_bool js##.isDirectory;
    is_file = Js.to_bool js##.isFile;
    is_symlink = Js.to_bool js##.isSymlink;
  }

let to_js t : js Js.t = object%js
  val mode = Js.string @@ Int32.to_string t.mode
  val uid = t.uid
  val gid = t.gid
  val atime = Js.string @@ Int64.to_string t.atime
  val ctime = Js.string @@ Int64.to_string t.ctime
  val mtime = Js.string @@ Int64.to_string t.mtime
  val size = Js.string @@ Int64.to_string t.size
  val isDirectory = Js.bool t.is_directory
  val isFile = Js.bool t.is_file
  val isSymlink = Js.bool t.is_symlink
end