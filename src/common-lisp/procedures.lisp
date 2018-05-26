;;; Define procedures of JSON-RPC
(in-package :cl-user)
(defpackage #:sxfiler/procedures
  (:use #:cl)
  (:import-from #:sxfiler/state
                #:with-root-state)
  (:import-from #:sxfiler/procedures/file-op)
  (:import-from #:sxfiler/procedures/pane-op)
  (:import-from #:sxfiler/procedures/completer-op)
  (:import-from #:sxfiler/procedures/config-op)
  (:import-from #:sxfiler/snapshot
                #:+snapshot-directory+
                #:+snapshot-file-path+)
  (:export #:expose-procedures))

(in-package #:sxfiler/procedures)

(defun take-snapshot-of-state (state)
  "Take the snapshot with STATE."
  (check-type state sxfiler/state:state)
  (ensure-directories-exist +snapshot-directory+ :mode #o755)
  (with-open-file (stream
                   +snapshot-file-path+
                   :direction :output
                   :if-exists :supersede)
    (sxfiler/snapshot:save-snapshot stream state)))

(defun expose-procedures (server)
  (sxfiler/procedures/file-op:expose server)
  (sxfiler/procedures/pane-op:expose server)
  (sxfiler/procedures/completer-op:expose server)
  (sxfiler/procedures/config-op:expose server)
  (jsonrpc:expose server "getCurrentState" (lambda (args)
                                             (declare (ignorable args))
                                             (with-root-state (state)
                                               state)))
  (jsonrpc:expose server "takeSnapshot" #'(lambda (args)
                                            (declare (ignorable args))
                                            (with-root-state (state)
                                              (take-snapshot-of-state state)))))
