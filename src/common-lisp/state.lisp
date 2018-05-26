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
           #:make-state
           #:state-active-pane
           #:state-inactive-pane
           #:state-active-pane-history
           #:state-inactive-pane-history

           ;; macro to access root state with parallel
           #:with-root-state

           #:initialize-pane-if-empty))
(in-package #:sxfiler/state)

(defstruct state
  "The state that is root of the all state of sxfiler.
The struct of root contains many status below.
"
  (active-pane (make-pane :location :left) :type pane)
  (active-pane-history (make-pane-history :location :left) :type pane-history)
  (inactive-pane (make-pane :location :right) :type pane)
  (inactive-pane-history (make-pane-history :location :right) :type pane-history))

(defmethod yason:encode ((object state) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "activePane" (state-active-pane object))
      (yason:encode-object-element "inactivePane" (state-inactive-pane object))
      (yason:encode-object-element "activePaneHistory" (state-active-pane-history object))
      (yason:encode-object-element "inactivePaneHistory" (state-inactive-pane-history object)))))

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

(defun initialize-pane-if-empty (directory)
  "Initialize panes of *ROOT-STATE* if pane's directory is empty."
  (check-type directory string)
  (flet ((initialize-pane (pane)
           (sxfiler/types/pane:renew-file-list pane :directory directory)))
    (let ((active-pane (state-active-pane *root-state*))
          (inactive-pane (state-inactive-pane *root-state*))
          (active-history (state-active-pane-history *root-state*))
          (inactive-history (state-inactive-pane-history *root-state*)))
      (when (string= "" (sxfiler/types/pane:pane-directory active-pane))
        (setf (state-active-pane *root-state*) (initialize-pane active-pane))
        (let ((pane (state-active-pane *root-state*)))
          (setf (state-active-pane-history *root-state*)
                (sxfiler/types/pane-history:push-record active-history
                                                        (sxfiler/types/pane-history:pane->record pane)))))
      (when (string= "" (sxfiler/types/pane:pane-directory inactive-pane))
        (setf (state-inactive-pane *root-state*) (initialize-pane inactive-pane))
        (let ((pane (state-inactive-pane *root-state*)))
          (setf (state-inactive-pane-history *root-state*)
                (sxfiler/types/pane-history:push-record inactive-history
                                                        (sxfiler/types/pane-history:pane->record pane))))))))
