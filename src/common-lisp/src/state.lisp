;;; Define struct for single-source-of-truth state.
(in-package #:cl-user)
(defpackage #:sxfiler/src/state
  (:use #:cl)
  (:nicknames #:sxfiler/state)
  (:import-from #:sxfiler/src/types/pane)
  (:import-from #:yason)
  (:export #:state
           #:state-active-pane
           #:state-left-pane
           #:state-right-pane

           ;; macro to access root state with parallel
           #:with-root-state))
(in-package #:sxfiler/src/state)

(defstruct state
  "The state that is root of the all state of sxfiler.
The struct of root contains many status below.
"
  (active-pane :left :type symbol)
  (left-pane (sxfiler/src/types/pane:make-pane) :type sxfiler/src/types/pane:pane)
  (right-pane (sxfiler/src/types/pane:make-pane) :type sxfiler/src/types/pane:pane))

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
