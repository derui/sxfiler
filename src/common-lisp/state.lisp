;;; Define struct for single-source-of-truth state.
(in-package #:cl-user)
(defpackage #:sxfiler/state
  (:use #:cl)
  (:import-from #:sxfiler/types/pane-history
                #:make-pane-history
                #:pane-history)
  (:import-from #:sxfiler/types/pane
                #:make-pane
                #:pane)
  (:export #:state
           #:state-active-pane
           #:state-left-pane
           #:state-right-pane
           #:state-left-pane-history
           #:state-right-pane-history

           ;; macro to access root state with parallel
           #:with-root-state

           #:swap-pane-location
           #:panes-of-state
           #:set-active-pane
           #:set-inactive-pane))
(in-package #:sxfiler/state)

(defstruct state
  "The state that is root of the all state of sxfiler.
The struct of root contains many status below.
"
  (active-pane :left :type symbol)
  (left-pane (make-pane) :type pane)
  (right-pane (make-pane) :type pane)
  (left-pane-history (make-pane-history) :type pane-history)
  (right-pane-history (make-pane-history) :type pane-history))

(defmethod yason:encode ((object state) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "activePane"
                                   (format nil "~(~A~)" (symbol-name (state-active-pane object))))
      (yason:encode-object-element "leftPane" (state-left-pane object))
      (yason:encode-object-element "rightPane" (state-right-pane object))
      (yason:encode-object-element "leftPaneHistory" (state-left-pane-history object))
      (yason:encode-object-element "rightPaneHistory" (state-right-pane-history object)))))

(defparameter *root-state* (make-state))

(defparameter *root-state-lock* (sb-thread:make-mutex))

(defmacro with-root-state ((var) &body body)
  `(if (and (sb-thread:holding-mutex-p *root-state-lock*)
            (eql sb-thread:*current-thread* (sb-thread:mutex-owner *root-state-lock*)))
       (let ((,var *root-state*))
         (progn
           ,@body))
       (sb-thread:with-mutex (*root-state-lock*)
         (let ((,var *root-state*))
           (progn
             ,@body)))))

;; State mutation functions
(defun swap-pane-location (pane)
  "Get swapped location given location."
  (check-type pane symbol)
  (ecase pane
    (:left :right)
    (:right :left)))

(defun set-inactive-pane (state pane)
  "Set PANE to inactive pane of STATE, and return modified STATE.
 This function has side effect to given STATE."
  (check-type state state)
  (check-type pane pane)

  (ecase (state-active-pane state)
    (:left (setf (state-right-pane state) pane))
    (:right (setf (state-left-pane state) pane)))
  state)

(defun set-active-pane (state pane)
  "Set PANE to active pane of STATE, and return modified STATE.
This function has side effect to given STATE."
  (check-type state state)
  (check-type pane pane)

  (ecase (state-active-pane state)
    (:left (setf (state-left-pane state) pane))
    (:right (setf (state-right-pane state) pane)))
  state)

(defun panes-of-state (state)
  "Get panes of active and inactive as multiple-values.
Order of values returned from this function is below.
active-pane, inactive-pane
"
  (check-type state state)
  (let ((active-pane (ecase (state-active-pane state)
                       (:left (state-left-pane state))
                       (:right (state-right-pane state))))
        (inactive-pane (ecase (state-active-pane state)
                         (:left (state-right-pane state))
                         (:right (state-left-pane state)))))
    (values active-pane inactive-pane)))
