open Abbrev

module Store_theme = struct
  type error =
    | Duplicate_name of string
    | Unknown_error  of string
  [@@deriving show, eq]

  type t = D.Theme.t -> (unit, error) result Lwt.t
  (** store theme *)
end

module Remove_theme = struct
  type error =
    | Not_found_theme of string
    | Unknown_error   of string
  [@@deriving show, eq]

  type t = D.Theme.t -> (unit, error) result Lwt.t
  (** remove theme *)
end

module List_theme = struct
  type t = unit -> D.Theme.t list Lwt.t
  (** list theme *)
end
