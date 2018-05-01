;;; exposable functions for JSON-RPC
(in-package :cl-user)
(defpackage #:sxfiler/procedures/dialog-op
  (:use #:cl)
  (:import-from #:sxfiler/state
                #:with-root-state
                #:state-dialog-state)
  (:import-from #:sxfiler/types/dialog-state
                #:dispatch-operation)
  (:import-from #:sxfiler/dialogs/confirmation
                #:confirmation-behavior)
  (:export #:expose))
(in-package #:sxfiler/procedures/dialog-op)

(defun handle-confirmation (state operation)
  "Handle operation for confirmation dialog."
  (check-type state sxfiler/state:state)
  (check-type operation (or null string))

  (let ((dialog-state (dispatch-operation (state-dialog-state state) operation)))
    (setf (state-dialog-state state) dialog-state)
    state))

(defun expose (server)
  (jsonrpc:expose server "/dialog/confirmation" (lambda (args)
                                                  (check-type args list)
                                                  (let ((operation (first args)))
                                                    (with-root-state (state)
                                                      (handle-confirmation state operation))))))
