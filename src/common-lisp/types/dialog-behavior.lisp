;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types/dialog-behavior
  (:use #:cl)
  (:export #:dialog-behavior
           #:make-dialog-behavior
           #:behave-with-operation
           #:undefined-operation-error
           ))
(in-package #:sxfiler/types/dialog-behavior)

;; Base structure for behavior of dialog
;; Include this structure if add new dialog.
(defstruct dialog-behavior
  (name "" :type string))

(defmethod behave-with-operation (behavior operation &rest args)
  "Dispatch OPERAITON to BEHAVIOR. Must return multi-value below.
  - close dialog or not
  - new behavior
")

(define-condition undefined-operation-error (error)
  ((name :initarg :name
         :reader undefined-operation-error-name))
  (:report (lambda (condition stream)
             (format stream "Undefined operation `~A'"
                     (undefined-operation-error-name condition)))))

(defmethod yason:encode ((object dialog-behavior) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "name" (dialog-behavior-name object)))))
