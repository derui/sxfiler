(** File module provides utility functions for file management such as creating temporary
    directory. *)

val mk_temp_dir : ?mode:int -> ?dir:string -> string -> string
(** [with_temp_dir ?mode ?dir pat] returns name of temporary directory in [dir]. Using
    {!Filename.get_temp_dir_name} is to make temporary directory if [dir] is not given. The default
    value of [mode] is [0o755]. *)

val remove : ?recursive:bool -> string -> unit
(** [remove ?recursive path] removes file or directory located [path]. The default value of
    [recursive] is true, then removes all structure in [path] and removes [path]. If [recursive] is
    false and [path] targeted a directory, this function fails with {!Sys.Sys_error}. *)
