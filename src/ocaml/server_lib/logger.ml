(** {!Logger} defines reporter and tags are used with {!Logs} *)
open Sxfiler_core

(** {!Tags} defines tag type and tag constructor to add tag to log message. *)
module Tags = struct
  let module_tag : string list Logs.Tag.def =
    Logs.Tag.def "module" ~doc:"Path of module hierarchy" @@ fun _ v ->
    Printf.printf "%s" (String.concat "." v)

  let module_main () = Logs.Tag.(empty |> add module_tag ["main"])
  let module_lib c = Logs.Tag.(empty |> add module_tag (["lib"] @ c))
end

(** [lwt_reporter ppf] returns reporter function to be able to assign to {!Logs.set_reporter}. *)
let lwt_reporter ppf =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let _, app_flush = buf_fmt ~like:Fmt.stdout in
  let _, dst_flush = buf_fmt ~like:Fmt.stderr in
  let report _ level ~over k msgf =
    let k _ =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    (* custom formater to contains timestamp and module name to log. *)
    let with_timestamp h tags k ppf fmt =
      let open Option.Infix in
      let module_path = match tags >>= Logs.Tag.find Tags.module_tag with
        | None -> "unknown"
        | Some list -> String.concat "." list
      in
      let now = Unix.gettimeofday () in
      let utc = Unix.gmtime now in
      let timestamp = Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03.0f" (utc.Unix.tm_year + 1900)
          (succ utc.Unix.tm_mon) utc.Unix.tm_mday utc.Unix.tm_hour utc.Unix.tm_min utc.Unix.tm_sec
          (1000.0 *. (now -. floor now)) in
      Format.kfprintf k ppf ("[%s]%a[%s] @[" ^^ fmt ^^ "@]@.") timestamp Logs.pp_header (level, h) module_path
    in
    msgf @@ fun ?header ?tags fmt -> with_timestamp header tags k ppf fmt
  in
  { Logs.report = report }
