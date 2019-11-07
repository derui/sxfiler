(** {!Logger} defines reporter and tags are used with {!Logs} *)

let base_src = "sxfiler"

(** [lwt_reporter ppf] returns reporter function to be able to assign to {!Logs.set_reporter}. *)
let lwt_reporter ppf =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b,
      fun () ->
        let m = Buffer.contents b in
        Buffer.reset b;
        m )
  in
  let _, app_flush = buf_fmt ~like:Fmt.stdout in
  let _, dst_flush = buf_fmt ~like:Fmt.stderr in
  let report src level ~over k msgf =
    let k _ =
      let write () =
        match level with
        | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () =
        over ();
        Lwt.return_unit
      in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    (* custom formater to contains timestamp and module name to log. *)
    let with_timestamp h src k ppf fmt =
      let now = Unix.gettimeofday () in
      let utc = Unix.gmtime now in
      let timestamp =
        Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03.0f" (utc.Unix.tm_year + 1900)
          (succ utc.Unix.tm_mon) utc.Unix.tm_mday utc.Unix.tm_hour utc.Unix.tm_min utc.Unix.tm_sec
          (1000.0 *. (now -. floor now))
      in
      Format.kfprintf k ppf
        ("[%s]%a[%s] @[" ^^ fmt ^^ "@]@.")
        timestamp Logs.pp_header (level, h) (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags:_ fmt -> with_timestamp header src k ppf fmt
  in
  { Logs.report }

(** make a new logger *)
let make module_path =
  let module_path = base_src :: module_path |> String.concat "." in
  let src = Logs.Src.create module_path in
  Logs_lwt.src_log src
