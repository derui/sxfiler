;;; Define struct for single-source-of-truth state.
(in-package #:cl-user)
(defpackage #:sxfiler/state
  (:use #:cl)
  (:import-from #:sxfiler/types)
  (:export #:root-state
           #:make-root-state

           #:*root-state*
           ;; updater for root state.
           #:update-root-state))
(in-package #:sxfiler/state)

(defstruct root-state
  "The state that is root of the all state of sxfiler.
The struct of root contains many status below.
"
  (active-pane :left :type symbol)
  (left-pane (sxfiler/types:make-pane) :type sxfiler/types:pane)
  (right-pane (sxfiler/types:make-pane) :type sxfiler/types:pane))

(defparameter *root-state* (make-root-state))

(defparameter *root-state-lock* (sb-thread:make-mutex))

(defmacro update-root-state (accessor &body body)
  "macro to update root state atomically.
All of manipulations for root state must use this all time.
"
  `(sb-thread:with-mutex (*root-state-lock*)
     (setf (funcall ,accessor *root-state*) (progn ,@body))))
