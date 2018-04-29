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
  (:import-from #:sxfiler/types/dialog-state
                #:make-dialog-state
                #:dialog-state)
  (:import-from #:sxfiler/completer
                #:completer
                #:make-completer)
  (:export #:state
           #:state-active-pane
           #:state-left-pane
           #:state-right-pane
           #:state-left-pane-history
           #:state-right-pane-history
           #:state-file-completion-state
           #:state-history-completion-state

           ;; macro to access root state with parallel
           #:with-root-state

           #:swap-pane-location))
(in-package #:sxfiler/state)

(defstruct state
  "The state that is root of the all state of sxfiler.
The struct of root contains many status below.
"
  (active-pane :left :type symbol)
  (left-pane (make-pane) :type pane)
  (right-pane (make-pane) :type pane)
  (left-pane-history (make-pane-history) :type pane-history)
  (right-pane-history (make-pane-history) :type pane-history)
  (dialog-state (make-dialog-state) :type dialog-state)
  (file-completion-state (make-completer :matcher (sxfiler/completer:get-matcher :partial))
                         :type completer)
  (history-completion-state (make-completer :matcher (sxfiler/completer:get-matcher :partial))
                            :type completer))

(defmethod yason:encode ((object state) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "activePane" (string-downcase
                                                 (symbol-name (state-active-pane object))))
      (yason:encode-object-element "leftPane" (state-left-pane object))
      (yason:encode-object-element "rightPane" (state-right-pane object))
      (yason:encode-object-element "leftPaneHistory" (state-left-pane-history object))
      (yason:encode-object-element "rightPaneHistory" (state-right-pane-history object))
      (yason:encode-object-element "dialogState" (state-dialog-state object))
      (yason:encode-object-element "fileCompletionState" (state-file-completion-state object))
      (yason:encode-object-element "historyCompletionState"
                                   (state-history-completion-state object)))))

(defparameter *root-state* (make-state))

(defparameter *root-state-lock* (sb-thread:make-mutex))

(defmacro with-root-state (var &body body)
  `(sb-thread:with-mutex (*root-state-lock*)
     (let ((,var *root-state*))
       (progn
         ,@body))))

;; State mutation functions
(defun swap-pane-location (pane)
  "Get swapped location given location."
  (check-type pane pane)
  (case pane
    (:left :right)
    (:right :left)
    (t :left)))
