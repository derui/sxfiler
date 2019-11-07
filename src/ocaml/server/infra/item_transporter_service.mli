(** Implementation for {!Sxfiler_domain.Item_transporter_service} *)
module Make
    (NS : Notification_service.S)
    (MF : Message_notification_factory.S)
    (PF : Progress_notification_factory.S) : Sxfiler_domain.Item_transporter_service.S
