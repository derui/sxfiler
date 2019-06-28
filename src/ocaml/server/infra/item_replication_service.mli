module Make
    (NS : Notification_service.S)
    (MF : Message_notification_factory.S)
    (PF : Progress_notification_factory.S) : Sxfiler_domain.Item_replication_service.S
(** Implementation for {!Sxfiler_domain.Item_replication_service} *)
