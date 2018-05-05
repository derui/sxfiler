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
  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (declare (ignorable inactive-pane))
    (let ((new-pane (enter-focused-directory active-pane)))
      (sxfiler/state:update-active-pane state new-pane))))

(defun up-directory (pane)
  "Up directory structure of PANE."
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
      (sxfiler/state:update-active-pane state new-pane))))

(defun reload-active-pane (state)
  "Reload file list of active pane"
  (check-type state sxfiler/state:state)
  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (declare (ignorable inactive-pane))
    (let ((new-pane (renew-file-list active-pane)))
      (sxfiler/state:update-active-pane state new-pane))))

(defun sync-to-inactive-pane (state)
  "Sync inactive pane and active pane to point same directory."
  (check-type state sxfiler/state:state)
  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (let ((new-pane (renew-file-list active-pane :directory (pane-directory inactive-pane))))
      (sxfiler/state:update-active-pane state new-pane))))

(defun toggle-mark-on-active-pane (state id)
  "Mark an item contained in active pane"
  (check-type state sxfiler/state:state)
  (check-type id string)
  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (declare (ignorable inactive-pane))
    (let ((new-pane (sxfiler/types/pane:toggle-mark active-pane id)))
      (sxfiler/state:update-active-pane state new-pane))))

(defun focus-item-on-active-pane (state id)
  "Focus an item specified with ID on active pane of STATE."
  (check-type state sxfiler/state:state)
  (check-type id string)
  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (declare (ignorable inactive-pane))
    (let ((item (find-if #'(lambda (v) (string= (sxfiler/types/file-stat:file-stat-id v) id))
                         (sxfiler/types/pane:pane-file-list active-pane))))
      (sxfiler/state:update-active-pane state (sxfiler/types/pane:focus-item active-pane item)))))

(defun expose (server)
  (check-type server jsonrpc:server)
  (jsonrpc:expose server "pane/focus" #'(lambda (args)
                                          (declare (type list args))
                                          (with-root-state (state)
                                            (focus-item-on-active-pane state (first args)))))
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
  (jsonrpc:expose server "pane/reloadPane" #'(lambda (args)
                                               (declare (ignorable args))
                                               (with-root-state (state)
                                                 (reload-active-pane state))))
  (jsonrpc:expose server "pane/enterDirectory" #'(lambda (args)
                                                   (declare (ignorable args))
                                                   (with-root-state (state)
                                                     (enter-directory state)))))
