module T = Sxfiler_domain

(** {!Make_sync} module defines interface to make scanner. *)
module Make_sync = struct
  type params = {
    initial_location: string;
    name: string;
  }

  type result = unit
  let name = "scanner/make/sync"
end

module Get_sync = struct
  type params = {
    name: string;
  }

  type result = T.Scanner.t
  let name = "scanner/get/sync"
end
