;;; Define struct for single-source-of-truth state.
(in-package #:cl-user)
(defpackage #:sxfiler/state
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:sxfiler/types/pane
                #:make-pane
                #:pane)
  (:export #:state
           #:state-active-pane
           #:state-left-pane
           #:state-right-pane

           ;; macro to access root state with parallel
           #:with-root-state))
(in-package #:sxfiler/state)

(defstruct state
  "The state that is root of the all state of sxfiler.
The struct of root contains many status below.
"
  (active-pane :left :type symbol)
  (left-pane (make-pane) :type pane)
  (right-pane (make-pane) :type pane))

(defmethod yason:encode ((object state) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "activePane" (string-downcase
                                                 (symbol-name (state-active-pane object))))
      (yason:encode-object-element "leftPane" (state-left-pane object))
      (yason:encode-object-element "rightPane" (state-right-pane object)))))

(defparameter *root-state* (make-state))

(defparameter *root-state-lock* (sb-thread:make-mutex))

(defmacro with-root-state (var &body body)
  `(sb-thread:with-mutex (*root-state-lock*)
     (let ((,var *root-state*))
       (progn
         ,@body))))
