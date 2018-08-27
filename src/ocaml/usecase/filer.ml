(** Use cases for filer *)
open Sxfiler_core
module T = Sxfiler_domain

module Make_type = struct
  type input = {
    initial_location: Path.t;
    name: string;
  }

  type output = T.Filer.t
end

(** {!Make_sync} module defines interface to make filer. *)
module type Make = sig
  (* trick to remove dependency for Make_type *)
  include module type of Make_type
  include Common.Usecase with type input := input
                          and type output := output
end

module Make
    (CR:T.Configuration.Repository)
    (SR:T.Filer.Repository)
    (NR:T.Node.Repository) : Make = struct
  include Make_type

  let execute (params:input) =
    (* Create filer if it is not exists *)
    let%lwt config = CR.resolve () in
    let%lwt v = SR.resolve params.name in
    match v with
    | None ->
      let sort_order = config.T.Configuration.default_sort_order in
      let%lwt nodes = NR.find_by_dir ~dir:params.initial_location in
      let t = T.Filer.make ~id:params.name ~location:params.initial_location
          ~nodes ~history:(T.Location_history.make ()) ~sort_order in
      let%lwt () = SR.store t in
      Lwt.return @@ Ok t
    | Some _ -> Lwt.return @@ Error (Common.MakeFilerError `Already_exists)
end

module Get_type = struct
  type input = {
    name: string;
  }
  type output = T.Filer.t
end

module type Get = sig
  include module type of Get_type
  include Common.Usecase with type input := input
                          and type output := output
end

module Get(SR:T.Filer.Repository) : Get = struct
  include Get_type

  let execute (params:input) =
    match%lwt SR.resolve params.name with
    | None -> Lwt.return @@ Error (Common.GetFilerError `Not_found)
    | Some filer -> Lwt.return @@ Ok filer
end
