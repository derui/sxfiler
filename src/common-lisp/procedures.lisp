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
  (:export #:expose-procedures))

(in-package #:sxfiler/procedures)

(defun expose-procedures (server)
  (sxfiler/procedures/file-op:expose server)
  (sxfiler/procedures/pane-op:expose server)
  (sxfiler/procedures/completer-op:expose server)
  (sxfiler/procedures/config-op:expose server)
  (jsonrpc:expose server "getCurrentState" (lambda (args)
                                             (declare (ignorable args))
                                             (with-root-state (state)
                                               state))))
