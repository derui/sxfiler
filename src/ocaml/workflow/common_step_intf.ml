open Sxfiler_core
open Abbrev

module File_list = struct
  type scan_location_error =
    [ `Not_exists    of Path.t
    | `Not_directory of Path.t
    ]

  type scan_location = Path.t -> (D.File_item.t list, scan_location_error) result Lwt.t

  type scan =
    D.File_list.unscanned ->
    (D.File_list.scanned Lwt.t, [ `Step_file_list_scan_location of scan_location S.Context.t ]) S.t
  (** workflow type to move location of a file_list [t] *)

  type reload =
    D.File_list.scanned ->
    (D.File_list.scanned Lwt.t, [ `Step_file_list_scan_location of scan_location S.Context.t ]) S.t
  (** workflow to reload a file_list [t] *)
end

module Completer = struct
  type provide_collection = unit -> D.Completer.collection Lwt.t
  (** collection provider *)

  type update_collection = D.Completer.collection -> unit Lwt.t
  (** collection updater *)

  type read =
    string ->
    ( D.Completer.candidates Lwt.t,
      [ `Step_completer_provide_collection of provide_collection S.Context.t
      | `Completer_instance                of (module D.Completer.Instance) S.Context.t
      ] )
    S.t
  (** signature to get candidates from collection with input *)
end

module Interaction = struct
  type demand_decision = D.Interaction.command -> D.Interaction.event Lwt.t
  (** signature to demand decision by user from work flow. *)
end

module Filer = struct
  type error = Canceled

  type reload_left =
    D.Filer.t ->
    (D.Filer.left_file_window Lwt.t, [ `Step_file_list_scan_location of File_list.scan_location S.Context.t ]) S.t

  type reload_right =
    D.Filer.t ->
    (D.Filer.right_file_window Lwt.t, [ `Step_file_list_scan_location of File_list.scan_location S.Context.t ]) S.t

  type request_copy_interaction =
    D.File_item.t ->
    ( (D.Interaction.Filer_copy_selected.t, error) Lwt_result.t,
      [ `Step_interaction_demand_decision of Interaction.demand_decision S.Context.t ] )
    S.t

  type request_move_interaction =
    D.File_item.t ->
    ( (D.Interaction.Filer_move_selected.t, error) Lwt_result.t,
      [ `Step_interaction_demand_decision of Interaction.demand_decision S.Context.t ] )
    S.t

  type request_delete_interaction =
    D.File_item.t ->
    ( (D.Interaction.Filer_delete_selected.t, error) Lwt_result.t,
      [ `Step_interaction_demand_decision of Interaction.demand_decision S.Context.t ] )
    S.t

  type operation_error =
    | Not_exists         of Path.t
    | No_permission      of string
    | Destination_exists of Path.t
    | Unknown            of string
  [@@deriving eq, show]

  type operation_input = {
    source : Path.t;
    dest : Path.t;
    overwrite : bool;
  }

  type get = unit -> D.Filer.t option Lwt.t
  (** Get filer in application *)

  type copy_item = operation_input -> (unit, operation_error) Lwt_result.t
  (** copy an item from source to dest. Return [Not_exists] if source or destination is not exist. When [dest] is
      directory, this function should copy [source] to destination with same name of it. *)

  type move_item = operation_input -> (unit, operation_error) Lwt_result.t
  (** move an item from source to dest. Return [Not_exists_source] if source is not exist. Return [Not_exists_dest] if
      [dest] is not exists. Whem [dest] is directory, this function should move [source] to destination with same name
      of it. *)

  type delete_item = D.File_item.t -> (unit, operation_error) Lwt_result.t
  (** delete an item that given path. *)
end

module Keymap = struct
  type resolve_keymap = unit -> D.Keymap.t Lwt.t
  (** step to resolve key map *)

  type store_keymap = D.Keymap.t -> unit Lwt.t
  (** step to store key map *)

  type load_error =
    [ `Not_found
    | `Illegal_keymap of string
    ]
  [@@deriving eq, show]

  type load_keymap = Path.t -> (D.Keymap.t, load_error) result Lwt.t
  (** step to load keymap from the place *)
end

module Theme = struct
  module Store_theme = struct
    type base_theme = D.Common.Not_empty_string.t

    type error = Unknown_error of string [@@deriving show, eq]

    type t =
      (D.Common.Not_empty_string.t * D.Theme.Color_code.t) list ->
      base_theme option ->
      (D.Theme.color_pairs, error) result Lwt.t
    (** store theme *)
  end

  module Get_current_theme = struct
    type t = unit -> D.Theme.color_pairs Lwt.t
    (** Get current theme *)
  end
end

module Configuration = struct
  type load = unit -> D.Configuration_store.t Lwt.t
  (** Type of step to load configuration *)

  type save = D.Configuration_store.t -> unit Lwt.t
  (** Type of step to save the configuration *)
end

module Common = struct
  type now = unit -> Time.t
  (** Common sub steps for workflow *)
end

module Location_history = struct
  type generate_record = Path.t -> (D.Location_history.Record.t, [ `Step_common_now of Common.now S.Context.t ]) S.t
  (** [generate_record location] returns new record *)
end
