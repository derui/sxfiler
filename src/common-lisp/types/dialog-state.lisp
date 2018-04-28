;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types/dialog-state
  (:use #:cl)
  (:export #:dialog-state

           #:dialog-open-p
           #:dialog-close-p
           #:unknown-dialog-type
           #:open-dialog
           ))
(in-package #:sxfiler/types/dialog-state)

;; Structure for dialog state which is open or not
(defstruct dialog-state
  (opened nil :type (or null t))
  (dialog-type nil :type (or null symbol)))

(defmethod yason:encode ((object dialog-state) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "opened" (dialog-state-opened object))
      (if (null (dialog-state-dialog-type object))
          (yason:encode-object-element "dialogType" 'yason:null)
          (yason:encode-object-element "dialogType"
                                       (string-downcase
                                        (princ-to-string (dialog-state-dialog-type object))))))))

(defun dialog-open-p (obj)
  "Predicate for dialog is opened"
  (check-type obj dialog-state)
  (and obj
       (dialog-state-opened obj)))

(defun dialog-close-p (obj)
  "Predicate for dialog is closed"
  (check-type obj dialog-state)
  (not (dialog-open-p obj)))

(define-condition unknown-dialog-type (error)
  ((kind :initarg :kind
         :reader unknown-dialog-type-kind))
  (:report (lambda (condition stream)
             (format stream "Invalid dialog type ~A" (unknown-dialog-type-kind condition)))))

(defun validate-dialog-kind (kind)
  "Validate kind of dialog that are defined dialog"
  (check-type kind symbol)
  (or (eql kind :confirmation)
      (eql kind :name-input)
      (eql kind :jump)
      (eql kind :history)
      (eql kind :change-permission)
      (error (make-condition 'unknown-dialog-type :kind kind))))

(defun open-dialog (obj type)
  "Get a new state that open the specified dialog TYPE"
  (check-type obj dialog-state)
  (check-type type symbol)
  (validate-dialog-kind type)

  (make-dialog-state :opened t
                     :dialog-type type))
