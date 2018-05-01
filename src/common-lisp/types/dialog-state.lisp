;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types/dialog-state
  (:use #:cl)
  (:import-from #:sxfiler/types/dialog-behavior)
  (:export #:dialog-state
           #:dispatch-operation

           #:dialog-open-p
           #:dialog-close-p
           #:open-dialog
           ))
(in-package #:sxfiler/types/dialog-state)

;; Structure for dialog state which is open or not
(defstruct dialog-state
  (opened nil :type (or null t))
  (behavior (sxfiler/types/dialog-behavior:make-dialog-behavior)
            :type sxfiler/types/dialog-behavior:dialog-behavior))

(defmethod yason:encode ((object dialog-state) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "opened" (dialog-state-opened object))
      (yason:encode-object-element "behavior" (dialog-state-behavior object)))))

(defun dispatch-operation (state operation)
  (check-type state dialog-state)
  (check-type operation (not null))

  (let ((cloned (copy-structure state)))
    (multiple-value-bind (opened behavior)
        (sxfiler/types/dialog-behavior:behave-with-operation (dialog-state-behavior state) operation)
      (setf (dialog-state-behavior cloned) behavior)
      (setf (dialog-state-opened cloned) opened)
      cloned)))

(defun dialog-open-p (obj)
  "Predicate for dialog is opened"
  (check-type obj dialog-state)
  (and obj
       (dialog-state-opened obj)))

(defun dialog-close-p (obj)
  "Predicate for dialog is closed"
  (check-type obj dialog-state)
  (not (dialog-open-p obj)))

(defun open-dialog (obj behavior)
  "Get a new state that open the specified dialog TYPE"
  (check-type obj dialog-state)
  (check-type behavior sxfiler/types/dialog-behavior:dialog-behavior)

  (make-dialog-state :opened t
                     :behavior behavior))
