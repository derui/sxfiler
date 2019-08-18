open Sxfiler_core
(** Use cases for filer *)

module T = Sxfiler_domain

module Make = struct
  module Type = struct
    type input =
      { initial_location : Path.t
      ; name : string }

    type output = T.Filer.t
    type error = [`Already_exists]
  end

  (** {!Make_sync} module defines interface to make filer. *)
  module type S = sig
    (* trick to avoid error to unbound record field *)
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make
      (CR : T.Configuration.Repository)
      (SR : T.Filer.Repository)
      (FC : T.Filer.Factory.S)
      (Svc : T.Location_scanner_service.S) : S = struct
    include Type

    let execute (params : input) =
      (* Create filer if it is not exists *)
      let%lwt config = CR.resolve () in
      let%lwt v = SR.resolve_by_name params.name in
      match v with
      | None ->
        let sort_order = config.T.Configuration.default_sort_order in
        let%lwt file_list = Svc.scan params.initial_location in
        let t = FC.create ~name:params.name ~file_list ~sort_order in
        let%lwt () = SR.store t in
        Lwt.return @@ Ok t
      | Some _ -> Lwt.return @@ Error `Already_exists
  end
end

module Get = struct
  module Type = struct
    type input = {name : string}
    type output = T.Filer.t
    type error = [`Not_found]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (SR : T.Filer.Repository) : S = struct
    include Type

    let execute (params : input) =
      match%lwt SR.resolve_by_name params.name with
      | None -> Lwt.return_error `Not_found
      | Some filer -> Lwt.return_ok filer
  end
end

module Jump_location = struct
  (* change current location of filer to new location *)
  module Type = struct
    type input =
      { name : string
      ; location : Path.t }

    type output = T.Filer.t
    type error = [`Not_found]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make
      (SR : T.Filer.Repository)
      (Svc : T.Location_scanner_service.S)
      (Clock : T.Location_record.Clock) : S = struct
    include Type

    let execute (params : input) =
      match%lwt SR.resolve_by_name params.name with
      | None -> Lwt.return_error `Not_found
      | Some filer ->
        let%lwt file_list = Svc.scan params.location in
        let filer' = T.Filer.move_location filer (module Clock) ~file_list in
        let%lwt () = SR.store filer' in
        Lwt.return_ok filer'
  end
end

module Move_parent = struct
  (* move parent location from current location of filer *)
  module Type = struct
    type input = {name : string}
    type output = T.Filer.t
    type error = [`Not_found]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make
      (SR : T.Filer.Repository)
      (Svc : T.Location_scanner_service.S)
      (Clock : T.Location_record.Clock) : S = struct
    include Type

    let execute (params : input) =
      match%lwt SR.resolve_by_name params.name with
      | None -> Lwt.return_error `Not_found
      | Some ({file_list; _} as filer) ->
        let parent_dir = Path.dirname_as_path file_list.location in
        let%lwt file_list = Svc.scan parent_dir in
        let filer' = T.Filer.move_location filer (module Clock) ~file_list in
        let%lwt () = SR.store filer' in
        Lwt.return_ok filer'
  end
end

module Enter_directory = struct
  (* move to location of the item in filer *)
  module Type = struct
    type input =
      { name : string
      ; item_id : string }

    type output = T.Filer.t

    type error =
      [ `Not_found_filer
      | `Not_found_item
      | `Not_directory ]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make
      (FR : T.Filer.Repository)
      (Svc : T.Location_scanner_service.S)
      (Clock : T.Location_record.Clock) : S = struct
    include Type

    let execute (params : input) =
      let%lwt filer = FR.resolve_by_name params.name in
      let item = Option.(filer >>= T.Filer.find_item ~id:params.item_id) in
      match (filer, item) with
      | None, _ -> Lwt.return_error `Not_found_filer
      | _, None -> Lwt.return_error `Not_found_item
      | Some filer, Some item ->
        if not item.stat.is_directory then Lwt.return_error `Not_directory
        else
          let%lwt new_file_list = Svc.scan item.full_path in
          let filer' = T.Filer.move_location filer (module Clock) ~file_list:new_file_list in
          let%lwt () = FR.store filer' in
          Lwt.return_ok filer'
  end
end

module Toggle_mark = struct
  module Type = struct
    type input =
      { name : string
      ; item_ids : T.File_item.id list }

    type output = T.Filer.t
    type error = [`Not_found]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (SR : T.Filer.Repository) : S = struct
    include Type

    let execute (params : input) =
      let%lwt filer = SR.resolve_by_name params.name in
      let filer =
        Option.(
          filer
          >>= lift
          @@ fun filer ->
          let marked, not_marked =
            List.partition
              (fun v -> T.Filer.Marked_item_set.mem v filer.T.Filer.marked_items)
              params.item_ids
          in
          let filer = T.Filer.remove_mark filer ~ids:marked in
          T.Filer.add_mark filer ~ids:not_marked)
      in
      match filer with
      | None -> Lwt.return_error `Not_found
      | Some filer ->
        let%lwt () = SR.store filer in
        Lwt.return_ok filer
  end
end

(** Make plan to move items between filers. *)
module Move = struct
  (* Make plan to move items in filer to the location of another filer. *)
  module Type = struct
    type input =
      { source : string
      ; dest : string
      ; item_ids : T.File_item.id list }

    type output =
      { task_id : T.Task_types.id
      ; task_name : string }

    type error =
      [ `Not_found of string
      | `Same_filer ]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module type Dependencies = sig
    module FR : T.Filer.Repository
    module TF : T.Task.Factory.S
    module TR : T.Task.Repository
    module Transport : T.Item_transporter_service.S
    module Scan : T.Location_scanner_service.S
  end

  (** The executor of this plan. *)
  module Executor (P : sig
      val source_filer : T.Filer.t
      val dest_filer : T.Filer.t
      val source_items : T.File_item.id list
    end)
      (Dep : Dependencies) : T.Task.Executor = struct
    let interaction = Lwt_mvar.create_empty ()
    let apply_interaction = `Apply (Lwt_mvar.put interaction)

    let execute {T.Task_types.Context.task_id} =
      let open Dep in
      let source_filer = P.source_filer and dest_filer = P.dest_filer in
      let suggest item =
        let suggestion =
          T.Task_interaction.Suggestion.
            { task_id
            ; item_name = Path.basename item.T.File_item.full_path
            ; suggestions = [Rename; Overwrite] }
        in
        (suggestion, Lwt_mvar.take interaction)
      in
      let items =
        P.source_items
        |> List.map (fun id -> T.Filer.find_item source_filer ~id)
        |> List.filter Option.is_some |> List.map Option.get_exn
      in
      let%lwt () = Dep.Transport.transport ~items ~suggest ~_to:dest_filer.file_list in
      let%lwt from_tree = Scan.scan source_filer.file_list.location
      and to_tree = Scan.scan dest_filer.file_list.location in
      let from_filer = T.Filer.update_list source_filer ~file_list:from_tree
      and to_filer = T.Filer.update_list dest_filer ~file_list:to_tree in
      Lwt.(join [FR.store from_filer; FR.store to_filer])
  end

  module Make (Dep : Dependencies) : S = struct
    include Type

    let execute (params : input) =
      let open Dep in
      let is_same_filer = params.source = params.dest in
      if is_same_filer then Lwt.return_error `Same_filer
      else
        let%lwt source = FR.resolve_by_name params.source
        and dest = FR.resolve_by_name params.dest in
        match (source, dest) with
        | None, _ -> Lwt.return_error (`Not_found params.source)
        | _, None -> Lwt.return_error (`Not_found params.dest)
        | Some source', Some dest' ->
          let module Executor =
            Executor
              (struct
                let source_filer = source'
                let dest_filer = dest'
                let source_items = params.item_ids
              end)
              (Dep)
          in
          let task = TF.create ~executor:(module Executor) in
          let%lwt () = TR.store task in
          Lwt.return_ok {task_id = task.id; task_name = "Move"}
  end
end

(** Make plan to delete items *)
module Delete = struct
  module Type = struct
    type input =
      { source : string
      ; item_ids : T.File_item.id list }

    type output = T.Task_types.id
    type error = [`Not_found of string]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module type Dependencies = sig
    module FR : T.Filer.Repository
    module TF : T.Task.Factory.S
    module TR : T.Task.Repository
    module Scan : T.Location_scanner_service.S
    module Trash : T.Item_trash_service.S
  end

  (** Use case to delete items *)
  module Executor (P : sig
      val filer : T.Filer.t
      val target_items : T.File_item.id list
    end)
      (Dep : Dependencies) : T.Task.Executor = struct
    let apply_interaction = `No_interaction

    let execute _ =
      let open Dep in
      let file_list = P.filer.file_list in
      let items =
        P.target_items
        |> List.map (fun v -> T.Filer.(find_item P.filer ~id:v))
        |> List.filter Option.is_some |> List.map Option.get_exn
      in
      let%lwt () = Trash.trash items in
      let%lwt file_list = Scan.scan file_list.T.File_list.location in
      T.Filer.update_list P.filer ~file_list |> FR.store
  end

  module Make (Dep : Dependencies) : S = struct
    include Type

    let execute (params : input) =
      let open Dep in
      let%lwt source = FR.resolve_by_name params.source in
      match source with
      | None -> Lwt.return_error (`Not_found params.source)
      | Some source ->
        let module E =
          Executor
            (struct
              let filer = source
              let target_items = params.item_ids
            end)
            (Dep)
        in
        let task = TF.create ~executor:(module E) in
        let%lwt () = TR.store task in
        Lwt.return_ok task.id
  end
end

(** Make plan to copy items to other filer. *)
module Copy = struct
  (* Make plan to move items in filer to the location of another filer. *)
  module Type = struct
    type input =
      { source : string
      ; dest : string
      ; item_ids : T.File_item.id list }

    type output = T.Task_types.id
    type error = [`Not_found of string]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module type Dependencies = sig
    module FR : T.Filer.Repository
    module TF : T.Task.Factory.S
    module TR : T.Task.Repository
    module Replicate : T.Item_replication_service.S
    module Scan : T.Location_scanner_service.S
  end

  (** The executor of this plan. *)
  module Executor (P : sig
      val source_filer : T.Filer.t
      val dest_filer : T.Filer.t
      val source_items : T.File_item.id list
    end)
      (Dep : Dependencies) : T.Task.Executor = struct
    let interaction = Lwt_mvar.create_empty ()
    let apply_interaction = `Apply (Lwt_mvar.put interaction)

    let execute {T.Task_types.Context.task_id} =
      let open Dep in
      let source_filer = P.source_filer and dest_filer = P.dest_filer in
      let suggest item =
        let suggestion =
          T.Task_interaction.Suggestion.
            { task_id
            ; item_name = Path.basename item.T.File_item.full_path
            ; suggestions = [Rename; Overwrite] }
        in
        (suggestion, Lwt_mvar.take interaction)
      in
      let items =
        P.source_items
        |> List.map (fun id -> T.Filer.find_item source_filer ~id)
        |> List.filter Option.is_some |> List.map Option.get_exn
      in
      let%lwt () = Dep.Replicate.replicate ~items ~suggest ~_to:dest_filer.file_list in
      let%lwt from_list = Scan.scan source_filer.file_list.location
      and to_list = Scan.scan dest_filer.file_list.location in
      let from_filer = T.Filer.update_list source_filer ~file_list:from_list
      and to_filer = T.Filer.update_list dest_filer ~file_list:to_list in
      Lwt.(join [FR.store from_filer; FR.store to_filer])
  end

  module Make (Dep : Dependencies) : S = struct
    include Type

    let execute (params : input) =
      let open Dep in
      let%lwt source = FR.resolve_by_name params.source
      and dest = FR.resolve_by_name params.dest in
      match (source, dest) with
      | None, _ -> Lwt.return_error (`Not_found params.source)
      | _, None -> Lwt.return_error (`Not_found params.dest)
      | Some source', Some dest' ->
        let module Executor =
          Executor
            (struct
              let source_filer = source'
              let dest_filer = dest'
              let source_items = params.item_ids
            end)
            (Dep)
        in
        let task = TF.create ~executor:(module Executor) in
        let%lwt () = TR.store task in
        Lwt.return_ok task.id
  end
end
