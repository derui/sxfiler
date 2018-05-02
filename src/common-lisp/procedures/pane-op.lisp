;;; exposable functions for operations of pane.
(in-package :cl-user)
(defpackage #:sxfiler/procedures/pane-op
  (:use #:cl)
  (:import-from #:sxfiler/state
                #:with-root-state
                #:panes-of-state)
  (:import-from #:sxfiler/types/pane
                #:pane
                #:pane-focused-item
                #:pane-directory
                #:pane-file-list
                #:renew-file-list)
  (:import-from #:sxfiler/types/file-stat
                #:file-stat-equal
                )
  (:export #:expose))
(in-package #:sxfiler/procedures/pane-op)

;; pane operations
(defun swap-active-pane (state)
  "swap active pane of state."
  (check-type state sxfiler/state:state)
  (let ((pane-loc (sxfiler/state:swap-pane-location (sxfiler/state:state-active-pane state))))
    (setf (sxfiler/state:state-active-pane state) pane-loc)
    state))

(defun enter-focused-directory (pane)
  (check-type pane pane)

  (flet ((renew-pane-with-new-directory (item pane)
           (let* ((filename (sxfiler/types/file-stat:file-stat-filename item))
                  (directory (sxfiler/types/file-stat:file-stat-directory item))
                  (path (uiop:merge-pathnames* filename directory))
                  (copied (copy-structure pane)))
             (setf (pane-directory copied) (namestring path))
             (renew-file-list copied :directory path))))
    (let* ((focused (pane-focused-item pane))
           (file-list (pane-file-list pane))
           (item (find-if (lambda (v) (file-stat-equal v focused))
                          file-list)))
      (if (and item
               (sxfiler/types/file-stat:file-stat-directory-p item))
          (renew-pane-with-new-directory item pane)
          pane))))

(defun enter-directory (state)
  "Enter into current focused directory of PANE. If focusing item is not directory,
do nothing.
 "
  (check-type state sxfiler/state:state)

  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (declare (ignorable inactive-pane))
    (let ((new-pane (enter-focused-directory active-pane)))
      (sxfiler/state:set-active-pane state new-pane))))

(defun up-directory (pane)
  (check-type pane pane)

  (let* ((dir (uiop:probe-file* (pane-directory pane) :truename t))
         (parent (uiop:pathname-parent-directory-pathname dir)))
    (renew-file-list pane :directory (namestring parent))))

(defun up-directory-of-active-pane (state)
  "Up directory from current directory of active pane."
  (check-type state sxfiler/state:state)

  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (declare (ignorable inactive-pane))
    (let ((new-pane (up-directory active-pane)))
      (sxfiler/state:set-active-pane state new-pane))))

(defun reload-active-pane (state)
  "Reload file list of active pane"
  (check-type state sxfiler/state:state)

  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (declare (ignorable inactive-pane))
    (let ((new-pane (renew-file-list active-pane)))
      (sxfiler/state:set-active-pane state new-pane))))

(defun sync-to-inactive-pane (state)
  "Sync inactive pane and active pane to point same directory."
  (check-type state sxfiler/state:state)

  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (let ((new-pane (renew-file-list active-pane :directory (pane-directory inactive-pane))))
      (sxfiler/state:set-active-pane state new-pane))))

(defun expose (server)
  (check-type server jsonrpc:server)

  (jsonrpc:expose server "/pane/syncPane" (lambda (args)
                                            (declare (ignorable args))
                                            (with-root-state (state)
                                              (sync-to-inactive-pane state))))

  (jsonrpc:expose server "/pane/upDirectory" (lambda (args)
                                               (declare (ignorable args))
                                               (with-root-state (state)
                                                 (up-directory-of-active-pane state))))

  (jsonrpc:expose server "/pane/reloadPane" (lambda (args)
                                              (declare (ignorable args))
                                              (with-root-state (state)
                                                (reload-active-pane state))))

  (jsonrpc:expose server "/pane/enterDirectory" (lambda (args)
                                                  (declare (ignorable args))
                                                  (with-root-state (state)
                                                    (enter-directory state))))

  (jsonrpc:expose server "/pane/swapActivePane" (lambda (args)
                                                  (declare (ignorable args))
                                                  (with-root-state (state)
                                                    (swap-active-pane state)))))
