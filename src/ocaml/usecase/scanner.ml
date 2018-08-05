(** Use cases for scanner *)
open Sxfiler_core
module T = Sxfiler_domain

module Make_type = struct
  type input = {
    initial_location: Path.t;
    name: string;
  }

  type output = T.Scanner.t
end

(** {!Make_sync} module defines interface to make scanner. *)
module type Make = sig
  (* trick to remove dependency for Make_type *)
  include module type of Make_type
  include Common.Usecase with type input := input
                          and type output := output
end

module Make
    (SR:T.Scanner.Repository)
    (NR:T.Node.Repository) : Make = struct
  include Make_type

  let execute (params:input) =
    (* Create scanner if it is not exists *)
    let%lwt v = SR.resolve params.name in
    match v with
    | None ->
      let%lwt nodes = NR.find_by_dir ~dir:params.initial_location in
      let t = T.Scanner.make ~id:params.name ~location:params.initial_location
          ~nodes ~history:(T.Location_history.make ()) in
      let%lwt () = SR.store t in
      Lwt.return @@ Ok t
    | Some _ -> Lwt.return @@ Error (Common.MakeScannerError `Already_exists)
end

module Get_type = struct
  type input = {
    name: string;
  }
  type output = T.Scanner.t
end

module type Get = sig
  include module type of Get_type
  include Common.Usecase with type input := input
                          and type output := output
end

module Get(SR:T.Scanner.Repository) : Get = struct
  include Get_type

  let execute (params:input) =
    match%lwt SR.resolve params.name with
    | None -> Lwt.return @@ Error (Common.GetScannerError `Not_found)
    | Some scanner -> Lwt.return @@ Ok scanner
end
