(in-package #:cl-user)
(defpackage #:sxfiler
  (:nicknames #:sxfiler/main)
  (:use #:cl
        #:sxfiler/procedures)
  (:import-from #:sxfiler/procedures/config-op)
  (:import-from #:sxfiler/config)
  (:export #:expose-procedures
           #:load-default-config))
(in-package #:sxfiler)

(defun load-default-config (&optional (config "default-config.lisp"))
  "Load default configuration."
  (let* ((current-dir (uiop:getcwd))
         (default-config-path (uiop:merge-pathnames* config current-dir)))
    (setf sxfiler/procedures/config-op:*config*
          (sxfiler/config:load-from-file (namestring default-config-path)))))
