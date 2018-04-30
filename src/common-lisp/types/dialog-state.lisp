;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types/dialog-state
  (:use #:cl)
  (:import-from #:sxfiler/types/dialog-behavior)
  (:export #:dialog-state
           #:dispatch-operation

           #:dialog-open-p
           #:dialog-close-p
           #:unknown-type-error
           #:open-dialog
           ))
(in-package #:sxfiler/types/dialog-state)

;; Structure for dialog state which is open or not
(defstruct dialog-state
  (opened nil :type (or null t))
  (type nil :type (or null symbol))
  (behavior (sxfiler/types/dialog-behavior:make-dialog-behavior)
            :type sxfiler/types/dialog-behavior:dialog-behavior))

(defmethod yason:encode ((object dialog-state) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "opened" (dialog-state-opened object))
      (if (null (dialog-state-type object))
          (yason:encode-object-element "dialogType" 'yason:null)
          (yason:encode-object-element "dialogType"
                                       (string-downcase
                                        (princ-to-string (dialog-state-type object)))))
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

(define-condition unknown-type-error (error)
  ((kind :initarg :kind
         :reader unknown-type-error-kind))
  (:report (lambda (condition stream)
             (format stream "Unknown dialog type ~A" (unknown-type-error-kind condition)))))

(defun ensure-dialog-kind (kind)
  "Validate kind of dialog that are defined dialog"
  (check-type kind symbol)
  (or (eql kind :confirmation)
      (eql kind :name-input)
      (eql kind :jump)
      (eql kind :history)
      (eql kind :change-permission)
      (error (make-condition 'unknown-type-error :kind kind))))

(defun open-dialog (obj type behavior)
  "Get a new state that open the specified dialog TYPE"
  (check-type obj dialog-state)
  (check-type type symbol)
  (check-type behavior sxfiler/types/dialog-behavior:dialog-behavior)
  (ensure-dialog-kind type)

  (make-dialog-state :opened t
                     :type type
                     :behavior behavior))
