(in-package #:cl-user)
(defpackage #:sxfiler
  (:nicknames #:sxfiler/main)
  (:use #:cl
        #:sxfiler/procedures)
  (:import-from #:sxfiler/procedures/config-op)
  (:import-from #:sxfiler/config)
  (:import-from #:sxfiler/state)
  (:import-from #:sxfiler/snapshot
                #:+snapshot-directory+
                #:+snapshot-file-path+)
  (:export #:expose-procedures
           #:load-default-config
           #:restore-latest-snapshot
           #:initialize-state))
(in-package #:sxfiler)

(defun load-default-config ()
  "Load default configuration."
  (sxfiler/procedures/config-op:load-config))

(defun restore-latest-snapshot ()
  "Restore snapshot if it is exists."
  (ensure-directories-exist +snapshot-directory+ :mode #o755)
  (when (uiop:probe-file* +snapshot-file-path+)
    (with-open-file (stream +snapshot-file-path+)
      (let ((restored-state (sxfiler/snapshot:restore-snapshot stream)))
        (sxfiler/state:with-root-state (state)
          (setf state restored-state))))))

(defun initialize-state ()
  "Initialize root state before start server."
  (sxfiler/state:initialize-pane-if-empty (namestring (user-homedir-pathname))))
