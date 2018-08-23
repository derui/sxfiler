(** This program generates root module for sxfiler_renderer_command.
    Generated module has some rule below.

    - Module having prefix "s_" is as static command.
    - Module having prefix "d_" is as dynamic command.
    - Generated module has module aliases dropped prefix above, and two functions to register static and dynamic commands
*)
open Sxfiler_core

let static_prefix = "s_"
let dynamic_prefix = "d_"

let find_modules ~dir ~prefix =
  let files = Sys.readdir dir |> Array.to_list in
  let regexp = Re.Posix.compile_pat ("^" ^ prefix ^ ".*.ml$") in

  List.filter (fun file -> Re.execp regexp file) files
  |> List.map (fun file -> Filename.chop_extension file)

let find_static_commands = find_modules ~prefix:static_prefix
let find_dymanic_commands = find_modules ~prefix:dynamic_prefix

let build_command_alias ~ppf ~command ~prefix =
  let regexp = Re.Posix.compile_pat ("^" ^ prefix) in
  let list = Re.split regexp command in
  match list with
  | [] -> Printf.eprintf "Prefix only file."
  | file :: _ -> begin
      Format.fprintf ppf "module %s = %s\n" (String.capitalize_ascii file) (String.capitalize_ascii command);
      Format.pp_print_newline ppf ()
    end

let build_command_aliases ~ppf static_commands dynamic_commands =

  List.iter (fun command ->
      build_command_alias ~command ~prefix:static_prefix ~ppf
    ) static_commands;
  List.iter (fun command ->
      build_command_alias ~command ~prefix:dynamic_prefix ~ppf
    ) dynamic_commands

let build_static_exporter ~ppf commands =
  Format.fprintf ppf "let expose_static registry = \n";
  let commands = List.rev commands in
  match commands with
  | [] -> Format.fprintf ppf "()\n[@@@@warning \"-27\"]"
  | head :: rest ->
    List.iter (fun command ->
        Format.fprintf ppf "let registry = %s.expose registry in\n" @@ String.capitalize_ascii command
      ) @@ List.rev rest;
    Format.fprintf ppf "%s.expose registry\n" @@ String.capitalize_ascii head

let build_dynamic_exporter ~ppf commands =
  Format.fprintf ppf "let expose_dynamic registry = \n";
  let commands = List.rev commands in
  match commands with
  | [] -> Format.fprintf ppf "()\n[@@@@warning \"-27\"]"
  | head :: rest ->
    List.iter (fun command ->
        Format.fprintf ppf "let registry = %s.expose registry;\n" @@ String.capitalize_ascii command
      ) @@ List.rev rest;
    Format.fprintf ppf "%s.expose registry\n" @@ String.capitalize_ascii head

let () =
  let dir = Sys.getcwd () in
  let output = ref "" in
  let arg_specs = [
    ("-o", Arg.String (fun v -> output := v), "output as root module");
  ] in
  Arg.parse arg_specs ignore "";

  let static_commands = find_static_commands ~dir
  and dynamic_commands = find_dymanic_commands ~dir in

  Fun.bracket ~setup:(fun () -> open_out !output )
    ~teardown:(fun chan -> close_out chan)
    (fun chan ->
       let ppf = Format.formatter_of_out_channel chan in
       build_command_aliases ~ppf static_commands dynamic_commands;
       build_static_exporter ~ppf static_commands;
       build_dynamic_exporter ~ppf dynamic_commands;
       Format.pp_print_flush ppf ()
    )
