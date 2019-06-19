(** Implementation for {!Sxfiler_domain.Node_transporter_service} *)
module Make
    (NS : Sxfiler_domain.Notification_service.S)
    (Factory : Sxfiler_domain.Notification.Factory) : Sxfiler_domain.Node_transporter_service.S
