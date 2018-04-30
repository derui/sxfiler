;;; exposable functions for JSON-RPC
(in-package :cl-user)
(defpackage #:sxfiler/procedures/file-op
  (:use #:cl)
  (:import-from #:sxfiler/state
                #:with-root-state
                #:state-left-pane
                #:state-right-pane
                #:panes-of-state)
  (:import-from #:sxfiler/types/dialog-behavior)
  (:import-from #:sxfiler/types/pane
                #:renew-file-list)
  (:import-from #:sxfiler/types/file-stat
                #:file-stat-equal
                #:file-stat-id)
  (:export #:expose))
(in-package #:sxfiler/procedures/file-op)

(defun copy-file (src dist)
  "simply coping file from SRC to DIST.
This function has side effect."
  (check-type src pathname)
  (check-type dist pathname)

  (uiop:copy-file (namestring src)
                  (namestring dist)))

(define-condition not-found-file-id-error (error)
  ((id :initarg :id
       :reader not-found-file-id-error-id))
  (:report (lambda (condition stream)
             (format stream "Not found File ID: ~A" (not-found-file-id-error-id condition)))))

(defun copy-file-to-another-pane (src-pane dest-pane)
  "copy file focusing on SRC-PANE to DEST-PANE. this function has side effect"
  (check-type src-pane sxfiler/types/pane:pane)
  (check-type dest-pane sxfiler/types/pane:pane)

  (let ((file-list (sxfiler/types/pane:pane-file-list src-pane))
        (focused-item (sxfiler/types/pane:pane-focused-item src-pane))
        (src-dir (sxfiler/types/pane:pane-directory src-pane))
        (dest-dir (sxfiler/types/pane:pane-directory dest-pane)))
    (flet ((find-file (item)
             (when item
               (find-if (lambda (v) (file-stat-equal item v)) file-list)))
           (copy-file* (item)
             (let ((filename (sxfiler/types/file-stat:file-stat-filename item)))
               (copy-file (uiop:merge-pathnames* filename src-dir)
                          (uiop:merge-pathnames* filename dest-dir)))))

      (let ((item (find-file focused-item)))
        (if item
            (copy-file* item)
            (error (make-condition 'not-found-file-id-error :id (file-stat-id focused-item))))))))

(defun copy-file-from-active-to-inactive (state)
  "copy file focusing on active pane of state to inactive pane of state."

  (check-type state sxfiler/state:state)
  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (copy-file-to-another-pane active-pane inactive-pane)
    (sxfiler/state:set-inactive-pane state (renew-file-list inactive-pane))))

(defun expose (server)
  "expose functions for file-related operations to JSON-RPC"
  (jsonrpc:expose server "/file/copyFile" (lambda (args)
                                            (declare (ignorable args))
                                            (with-root-state state
                                              (copy-file-from-active-to-inactive state)))))
