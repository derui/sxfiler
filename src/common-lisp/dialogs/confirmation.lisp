;;; Provide this package of dialog behavior
(in-package :cl-user)
(defpackage #:sxfiler/dialogs/confirmation
  (:use #:cl)
  (:import-from #:sxfiler/types/dialog-behavior
                #:dialog-behavior
                #:behave-with-operation
                #:undefined-operation-error)
  (:export #:confirmation-behavior
           #:make-confirmation-behavior))
(in-package #:sxfiler/dialogs/confirmation)

;; structure of confirmation dialog.
;; HANDLE is a function when confirmed dialog, and should return t or nil
;; as dialog still opening or closing
(defstruct (confirmation-behavior (:include dialog-behavior (name "confirmation")))
  (handle nil :type (or nil function)))

(defmethod behave-with-operation ((behavior confirmation-behavior) operation &rest args)
  (check-type operation string)

  (cond
    ((and (string= "confirm" operation)
          (functionp (confirmation-behavior-handle behavior)))
     (values (apply (confirmation-behavior-handle behavior) args)
             behavior))
    ((string= "cancel" operation)
     (values nil behavior))
    (t (error (make-condition 'undefined-operation-error :name operation)))))
