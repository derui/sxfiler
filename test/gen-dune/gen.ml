(** This program generates configurations for each test executable with template configurations as dune.inc.
*)

open Sxfiler_core
open Ppx_sexp_conv_lib.Conv

type item =
  { name : string
  ; libraries : string list sexp_option
  ; flags : string list sexp_option
  ; preprocess : string list sexp_option }
[@@deriving sexp]

type configuration =
  { items : item list
  ; libraries : string list
  ; flags : string list [@sexp.omit_nil]
  ; preprocess : string list [@sexp.omit_nil] }
[@@deriving sexp]

type executable = Ppx_sexp_conv_lib.Sexp.t list [@@deriving sexp]
type alias = Ppx_sexp_conv_lib.Sexp.t list [@@deriving sexp]

let read_inc_file file =
  let ic = open_in file in
  let buffer = Buffer.create (in_channel_length ic) in
  let rec read_line ic buf =
    try
      let line = input_line ic in
      Buffer.add_string buf line ; Buffer.add_char buf '\n' ; read_line ic buf
    with End_of_file -> close_in ic ; Buffer.contents buf
  in
  read_line ic buffer

let make_executables conf =
  let to_list = Option.get ~default:(Fun.const []) in
  let to_executable item =
    let name = ("name", item.name)
    and modules = ("modules", item.name)
    and libraries = "libraries" :: List.concat [conf.libraries; to_list item.libraries]
    and flags = List.concat [conf.flags; to_list item.flags]
    and preprocess = List.concat [conf.preprocess; to_list item.preprocess] in
    let result =
      [ sexp_of_string "executable"
      ; [%sexp_of: string * string] name
      ; [%sexp_of: string * string] modules
      ; [%sexp_of: string list] libraries ]
    in
    let result =
      match flags with
      | [] -> result
      | _ as flags -> result @ [[%sexp_of: string list] ("flags" :: flags)]
    in
    match preprocess with
    | [] -> result
    | _ as preprocess -> result @ [[%sexp_of: string * string list] ("preprocess", preprocess)]
  in
  List.map to_executable conf.items

let make_aliases conf =
  let to_alias item =
    let name = ("name", "runtest")
    and deps = ("deps", item.name ^ ".exe")
    and action = ("action", ["run"; "%{deps}"]) in
    [ sexp_of_string "alias"
    ; [%sexp_of: string * string] name
    ; [%sexp_of: string * string] deps
    ; [%sexp_of: string * string list] action ]
  in
  List.map to_alias conf.items

let () =
  let inc_file = ref "" in
  let arg_specs = [] in
  Arg.parse arg_specs (fun v -> inc_file := v) "" ;
  let inc_content = read_inc_file !inc_file in
  let confs = Fun.(Sexplib.Sexp.of_string %> configuration_of_sexp) inc_content in
  let exec_to_string = Fun.(sexp_of_executable %> Sexplib.Sexp.to_string_mach) in
  let alias_to_string = Fun.(sexp_of_alias %> Sexplib.Sexp.to_string_mach) in
  let print_executable_from_conf =
    Fun.(
      make_executables
      %> List.iter (fun executable -> Printf.printf "%s\n" (exec_to_string executable)))
  in
  let print_alias_from_conf =
    Fun.(make_aliases %> List.iter (fun alias -> Printf.printf "%s\n" (alias_to_string alias)))
  in
  print_executable_from_conf confs ; print_alias_from_conf confs
