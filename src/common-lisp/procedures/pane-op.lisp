;;; exposable functions for operations of pane.
(in-package :cl-user)
(defpackage #:sxfiler/procedures/pane-op
  (:use #:cl)
  (:import-from #:sxfiler/state
                #:with-root-state
                #:state-active-pane
                #:state-inactive-pane)
  (:import-from #:sxfiler/types/pane
                #:pane
                #:pane-focused-item
                #:pane-directory
                #:pane-file-list
                #:renew-file-list
                #:find-focused-item)
  (:import-from #:sxfiler/types/file-stat
                #:file-stat-equal
                )
  (:export #:expose))
(in-package #:sxfiler/procedures/pane-op)

(defun enter-focused-directory (pane)
  "If focusing directory on PANE, move focusing directory and renew PANE.
This function do not mutate PANE, then returns new pane if focused directory.
"
  (check-type pane pane)
  (flet ((renew-pane-with-new-directory (item pane)
           (let* ((filename (sxfiler/types/file-stat:file-stat-filename item))
                  (directory (sxfiler/types/file-stat:file-stat-directory item))
                  (path (uiop:merge-pathnames* filename directory)))
             (renew-file-list pane :directory path))))
    (let ((item (find-focused-item pane)))
      (if (and item
               (sxfiler/types/file-stat:file-stat-directory-p item))
          (renew-pane-with-new-directory item pane)
          pane))))

(defun enter-directory (state)
  "Enter into current focused directory of PANE. If focusing item is not directory,
do nothing.
 "
  (check-type state sxfiler/state:state)
  (let ((active-pane (state-active-pane state)))
    (let ((new-pane (enter-focused-directory active-pane)))
      (setf (state-active-pane state) new-pane)
      state)))

(defun up-directory (pane)
  "Up directory structure of PANE."
  (check-type pane pane)
  (let* ((dir (uiop:probe-file* (pane-directory pane) :truename t))
         (parent (uiop:pathname-parent-directory-pathname dir)))
    (renew-file-list pane :directory (namestring parent))))

(defun up-directory-of-active-pane (state)
  "Up directory from current directory of active pane."
  (check-type state sxfiler/state:state)
  (let ((active-pane (state-active-pane state)))
    (let ((new-pane (up-directory active-pane)))
      (setf (state-active-pane state) new-pane)
      state)))

(defun reload-active-pane (state)
  "Reload file list of active pane"
  (check-type state sxfiler/state:state)
  (let ((active-pane (state-active-pane state)))
    (let ((new-pane (renew-file-list active-pane)))
      (setf (state-active-pane state) new-pane)
      state)))

(defun sync-to-inactive-pane (state)
  "Sync inactive pane and active pane to point same directory."
  (check-type state sxfiler/state:state)
  (let ((active-pane (state-active-pane state))
        (inactive-pane (state-inactive-pane state)))
    (let ((new-pane (renew-file-list active-pane :directory (pane-directory inactive-pane))))
      (setf (state-active-pane state) new-pane)
      state)))

(defun toggle-mark-on-active-pane (state id)
  "Mark an item contained in active pane"
  (check-type state sxfiler/state:state)
  (check-type id string)
  (let ((active-pane (state-active-pane state)))
    (let ((new-pane (sxfiler/types/pane:toggle-mark active-pane id)))
      (setf (state-active-pane state) new-pane)
      state)))

(defun move-focus-on-active-pane (state amount)
  "Move current focus with AMOUNT on active pane of STATE."
  (check-type state sxfiler/state:state)
  (check-type amount integer)
  (let ((active-pane (state-active-pane state)))
    (setf (state-active-pane state) (sxfiler/types/pane:move-focus active-pane amount))
    state))

(defun jump-directory-on-active-pane (state directory)
  "Jump to directory of active pane"
  (check-type state sxfiler/state:state)
  (check-type directory string)
  (let ((active-pane (state-active-pane state)))
    (setf (state-active-pane state) (sxfiler/types/pane:renew-file-list active-pane
                                                                        :directory directory))
    state))

(defun expose (server)
  (check-type server jsonrpc:server)
  (jsonrpc:expose server "pane/moveFocus" #'(lambda (args)
                                              (declare (type list args))
                                              (with-root-state (state)
                                                (move-focus-on-active-pane state (first args)))))
  (jsonrpc:expose server "pane/toggleMark" #'(lambda (args)
                                               (declare (type list args))
                                               (with-root-state (state)
                                                 (toggle-mark-on-active-pane state (first args)))))
  (jsonrpc:expose server "pane/syncPane" #'(lambda (args)
                                             (declare (ignorable args))
                                             (with-root-state (state)
                                               (sync-to-inactive-pane state))))
  (jsonrpc:expose server "pane/upDirectory" #'(lambda (args)
                                                (declare (ignorable args))
                                                (with-root-state (state)
                                                  (up-directory-of-active-pane state))))
  (jsonrpc:expose server "pane/reload" #'(lambda (args)
                                           (declare (ignorable args))
                                           (with-root-state (state)
                                             (reload-active-pane state))))
  (jsonrpc:expose server "pane/enterDirectory" #'(lambda (args)
                                                   (declare (ignorable args))
                                                   (with-root-state (state)
                                                     (enter-directory state))))
  (jsonrpc:expose server "pane/jump" #'(lambda (args)
                                         (declare (type list args))
                                         (with-root-state (state)
                                           (jump-directory-on-active-pane state (first args))))))
