(* application options *)
type t =
  { migemo_dict_dir : string
  ; configuration : string
  ; stat_file : string
  ; debug : bool }

let parse executable_dir =
  let dict_dir = ref "" in
  let config_dir = ref "" in
  let stat_file = ref Filename.(concat executable_dir "sxfiler_stat.json") in
  let debug = ref false in
  let arg_specs =
    [ ("-d", Arg.String (fun v -> dict_dir := v), "Directory of migemo dictionary")
    ; ( "--config"
      , Arg.String (fun v -> config_dir := v)
      , "The directory path for server configuration" )
    ; ("--stat_file", Arg.String (fun v -> stat_file := v), "File path for stat file")
    ; ("--debug", Arg.Unit (fun () -> debug := true), "Verbose mode") ]
  in
  Arg.parse arg_specs ignore "" ;
  {migemo_dict_dir = !dict_dir; configuration = !config_dir; debug = !debug; stat_file = !stat_file}
