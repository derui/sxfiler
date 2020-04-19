open Sxfiler_core

(* application options *)
type t = {
  migemo_dict_dir : string;
  configuration : string;
  stat_file : string;
  keymap_file : Path.t;
  config_file : string;
  initial_loc : string;
  port : int;
  debug : bool;
}

let default_keymap_file = "keymap.json"

let parse executable_dir =
  let dict_dir = ref "" in
  let config_dir = ref "" in
  let stat_file = ref Filename.(concat executable_dir "sxfiler_stat.json") in
  let keymap_file = ref default_keymap_file in
  let config_file = ref "config.json" in
  let debug = ref false in
  let port = ref 50789 in
  let initial_loc = ref (Sys.getenv_opt "HOME" |> Option.value ~default:".") in
  let arg_specs =
    [
      ("-d", Arg.String (fun v -> dict_dir := v), "Directory of migemo dictionary");
      ("--config", Arg.String (fun v -> config_dir := v), "The path of directory that contains server configuration");
      ("--stat_file", Arg.String (fun v -> stat_file := v), "File path for stat file");
      ("--keymap_file", Arg.String (fun v -> keymap_file := v), "File name of key map file");
      ("--config_file", Arg.String (fun v -> config_file := v), "File name of configuration file");
      ("--initial_loc", Arg.String (fun v -> initial_loc := v), "Initial location when stat file is not created");
      ("--debug", Arg.Unit (fun () -> debug := true), "Verbose mode");
      ("--port", Arg.Int (fun v -> port := v), "The port to run server with");
    ]
  in
  Arg.parse arg_specs ignore "";
  {
    migemo_dict_dir = !dict_dir;
    configuration = !config_dir;
    debug = !debug;
    keymap_file =
      Filename.(concat !config_dir !keymap_file)
      |> Path.of_string
      |> Result.fold ~ok:Fun.ident ~error:(fun _ ->
             Filename.(concat !config_dir default_keymap_file) |> Path.of_string |> Result.get_ok);
    config_file = !config_file;
    stat_file = !stat_file;
    initial_loc = !initial_loc;
    port = !port;
  }
