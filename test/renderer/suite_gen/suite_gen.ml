(**
   This program generate test suite including all modules in current directory that are
   matched prefix and suffix specified when running. Mocha_of_ocaml is test runner for modules collecting
   from this program.

   All modules collected by this program should have [suite] function having argument only [()].
*)
open Sxfiler_core

(* collect test cases in directory having suffix and prefix *)
let collect_test_cases dir ~suffix ~prefix ~output =
  let files = Sys.readdir dir |> Array.to_list in
  Logs.debug (fun m -> m "Regexp: %s" (Printf.sprintf "^%s.[^.]+%s.ml$" prefix suffix));
  let regexp = Re.Posix.compile_pat (Printf.sprintf "^%s.[^.]+%s.ml$" prefix suffix) in

  (* ignore output module to avoid unknown module error. *)
  List.filter (fun file -> file <> output) files
  |> List.filter (fun file ->
      Logs.debug (fun m -> m "Found file: %s" file);
      Re.execp regexp file)
  |> List.map (fun file ->
      Logs.debug (fun m -> m "Matched file: %s" file);
      Filename.chop_extension file)
  |> List.map String.capitalize_ascii

let write_suite ~ppf files =
  Format.fprintf ppf "let () =\n";
  List.iter (fun module_name ->
      Format.fprintf ppf "%s.suite ();\n" module_name
    ) files

let () =
  let dir = Sys.getcwd () in
  let suffix = ref "" in
  let prefix = ref "" in
  let output = ref "" in
  let arg_spec = [
    ("-s", Arg.String (fun v -> suffix := v), "Suffix for test case");
    ("-p", Arg.String (fun v -> prefix := v), "Prefix for test case");
    ("-o", Arg.String (fun v -> output := v), "output as root module");

  ] in
  Arg.parse arg_spec ignore "";
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.set_level (Some Logs.Info);

  Logs.debug (fun m -> m "Collect module in %s, having suffix:%s, prefix:%s" dir !suffix !prefix);

  Fun.bracket ~setup:(fun () -> open_out !output)
    ~teardown:(fun chan -> close_out chan)
    (fun chan ->
       let ppf = Format.formatter_of_out_channel chan in
       let module_files = collect_test_cases dir ~suffix:!suffix ~prefix:!prefix ~output:!output in
       write_suite ~ppf module_files;
       Format.pp_print_flush ppf ()
    )
