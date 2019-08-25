(** this module defines functions for procedures for keymap. *)

module G = Sxfiler_server_gateway
module P = Procedure

(* define procedure to list all bookmark registered *)
module List_all_spec (G : G.Bookmark.List_all.S) : P.Spec = struct
  module Gateway = G

  let method_ = "bookmark/listAll"
  let param_requirement = `Not_required ()
end

(* defines procedure to register a bookmark *)
module Register_spec (G : G.Bookmark.Register.S) : P.Spec = struct
  module Gateway = G

  let method_ = "bookmark/register"
  let param_requirement = `Required
end

(* defines procedure to delete the bookmark *)
module Delete_spec (G : G.Bookmark.Delete.S) : P.Spec = struct
  module Gateway = G

  let method_ = "bookmark/delete"
  let param_requirement = `Required
end
