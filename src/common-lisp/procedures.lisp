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

(defun swap-active-pane (state)
  (let ((pane-loc (sxfiler/state:swap-pane-location (sxfiler/state:state-active-pane state))))
    (setf (sxfiler/state:state-active-pane state) pane-loc)
    (copy-structure state)))

(defun expose-procedures (server)
  (jsonrpc:expose server "/getCurrentState" (lambda (args)
                                              (declare (ignorable args))
                                              (with-root-state state
                                                (get-current-state state))))

  (jsonrpc:expose server "/swapActivePane" (lambda (args)
                                             (declare (ignorable args))
                                             (with-root-state state
                                               (swap-active-pane state)))))
