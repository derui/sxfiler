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
           #:restore-latest-snapshot))
(in-package #:sxfiler)

(defun load-default-config (&optional (config "default-config.lisp"))
  "Load default configuration."
  (let* ((current-dir (uiop:getcwd))
         (default-config-path (uiop:merge-pathnames* config current-dir)))
    (setf sxfiler/procedures/config-op:*config*
          (sxfiler/config:load-from-file (namestring default-config-path)))))

(defun restore-latest-snapshot ()
  "Restore snapshot if it is exists."
  (ensure-directories-exist +snapshot-directory+ :mode #o755)
  (when (uiop:probe-file* +snapshot-file-path+)
    (with-open-file (stream snapshot-path)
      (let ((restored-state (sxfiler/snapshot:restore-snapshot stream)))
        (sxfiler/state:with-root-state (state)
          (setf state restored-state))))))
