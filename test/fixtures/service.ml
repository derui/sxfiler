(** Provide dummy implementation services for tests that uses service needed for functor/function
    arguments. *)

module D = Sxfiler_domain

module Node_transport_service : D.Node_transporter_service.S = struct
  let transport ~suggest:_ ~nodes:_ ~_to:_ = Lwt.return_unit
end

module Node_trash_service : D.Node_trash_service.S = struct
  let trash _ = Lwt.return_unit
end

let location_scanner_service location nodes =
  ( module struct
    let scan _ = D.File_tree.make ~location ~nodes |> Lwt.return
  end
  : D.Location_scanner_service.S )
