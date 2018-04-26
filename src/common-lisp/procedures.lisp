;;; Define procedures of JSON-RPC
(in-package :cl-user)
(defpackage #:sxfiler/procedures
  (:use #:cl)
  (:import-from #:sxfiler/state
                #:with-root-state)
  (:export #:expose-procedures))

(in-package #:sxfiler/procedures)

(defun get-current-state (state)
  "Return whole state that is copied from original"
  (copy-structure state))

(defun expose-procedures (server)
  (jsonrpc:expose server "/current-state" (lambda (args)
                                            (declare (ignorable args))
                                            (with-root-state state
                                              (get-current-state state)))))
