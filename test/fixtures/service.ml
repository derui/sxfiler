(** Provide dummy implementation services for tests that uses service needed for functor/function
    arguments. *)

module D = Sxfiler_domain

module Item_transport_service : D.Item_transporter_service.S = struct
  let transport ~suggest:_ ~items:_ ~_to:_ = Lwt.return_unit
end

module Item_trash_service : D.Item_trash_service.S = struct
  let trash _ = Lwt.return_unit
end

let location_scanner_service location items =
  ( module struct
    let scan _ = D.File_list.make ~location ~items () |> Lwt.return
  end
  : D.Location_scanner_service.S )
