;;; Define procedures of JSON-RPC
(in-package :cl-user)
(defpackage #:sxfiler/src/procedures
  (:nicknames #:sxfiler/procedures)
  (:use #:cl)
  (:import-from #:sxfiler/src/state
                #:with-root-state)
  (:export #:expose-procedures))

(in-package #:sxfiler/procedures)

(defun get-all-state (state)
  "Return whole state that is copied from original"
  (copy-structure state))

(defun expose-procedures (server)
  (jsonrpc:expose server "get-all-state" (lambda (args)
                                           (declare (ignorable args))
                                           (with-root-state state
                                             (get-all-state state)))))
