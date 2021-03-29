open Sxfiler_core
open Abbrev

module File_list = struct
  type scan_location_error =
    [ `Not_exists    of Path.t
    | `Not_directory of Path.t
    ]

  module type Instance = sig
    val scan_location : Path.t -> (D.File_item.t list, scan_location_error) result Lwt.t
  end
end

module Completer = struct
  module type Instance = sig
    val provide_collection : unit -> D.Completer.collection Lwt.t
    (** collection provider *)

    val update_collection : D.Completer.collection -> unit Lwt.t
    (** collection updater *)
  end
end

module Interaction = struct
  module type Instance = sig
    val demand_decision : D.Interaction.command -> D.Interaction.event Lwt.t
    (** signature to demand decision by user from work flow. *)
  end
end

module Filer = struct
  type error = Canceled

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

  module type Instance = sig
    val get : unit -> D.Filer.t option Lwt.t
    (** Get filer in application *)

    val copy_item : operation_input -> (unit, operation_error) Lwt_result.t
    (** copy an item from source to dest. Return [Not_exists] if source or destination is not exist. When [dest] is
        directory, this function should copy [source] to destination with same name of it. *)

    val move_item : operation_input -> (unit, operation_error) Lwt_result.t
    (** move an item from source to dest. Return [Not_exists_source] if source is not exist. Return [Not_exists_dest] if
        [dest] is not exists. Whem [dest] is directory, this function should move [source] to destination with same name
        of it. *)

    val delete_item : D.File_item.t -> (unit, operation_error) Lwt_result.t
    (** delete an item that given path. *)
  end
end

module Keymap = struct
  type load_error =
    [ `Not_found
    | `Illegal_keymap of string
    ]
  [@@deriving eq, show]

  module type Instance = sig
    val resolve_keymap : unit -> D.Keymap.t Lwt.t
    (** step to resolve key map *)

    val store_keymap : D.Keymap.t -> unit Lwt.t
    (** step to store key map *)

    val load_keymap : Path.t -> (D.Keymap.t, load_error) result Lwt.t
    (** step to load keymap from the place *)
  end
end

module Configuration = struct
  module type Instance = sig
    val load : unit -> D.Configuration_store.t Lwt.t
    (** Type of step to load configuration *)

    val save : D.Configuration_store.t -> unit Lwt.t
    (** Type of step to save the configuration *)
  end
end

module Common = struct
  module type Instance = sig
    val now : unit -> Time.t
    (** Common sub steps for workflow *)
  end
end

module Theme = struct
  type base_theme = D.Common.Not_empty_string.t

  type error = Unknown_error of string [@@deriving show, eq]

  module type Instance = sig
    val get_current_theme : unit -> D.Theme.color_pairs Lwt.t
    (** Get current theme *)

    val store_theme :
      (D.Common.Not_empty_string.t * D.Theme.Color_code.t) list ->
      base_theme option ->
      (D.Theme.color_pairs, error) result Lwt.t
    (** store theme *)
  end
end
