;;; exposable functions for JSON-RPC
(in-package :cl-user)
(defpackage #:sxfiler/procedures/file-op
  (:use #:cl)
  (:import-from #:sxfiler/state
                #:with-root-state
                #:state-left-pane
                #:state-right-pane
                #:panes-of-state)
  (:import-from #:sxfiler/types/pane
                #:pane-directory
                #:renew-file-list)
  (:import-from #:sxfiler/types/file-stat
                #:file-stat-equal
                #:file-stat-id)
  (:export #:expose))
(in-package #:sxfiler/procedures/file-op)

;; Conditions
(define-condition not-found-file-id-error (error)
  ((id :initarg :id
       :reader not-found-file-id-error-id))
  (:documentation "Raise when not found any file ID")
  (:report (lambda (condition stream)
             (format stream "Not found File ID: ~A" (not-found-file-id-error-id condition)))))

;; Primitive functions are used with pathname.
(defun copy-file (src dest)
  "simply coping a file from SRC to DIST.
This function has side effect."
  (check-type src pathname)
  (check-type dest pathname)
  (uiop:copy-file (namestring src)
                  (namestring dest)))

(defun move-file (src dest &key (overwrite t))
  "simply move a file from SRC to DIST. This function has side effect."
  (check-type src pathname)
  (check-type dest pathname)
  (uiop:rename-file-overwriting-target src dest))

;; functions are related current focusing item.
(defun copy-file-to-another-pane (src-pane dest-pane)
  "copy file focusing on SRC-PANE to DEST-PANE. this function has side effect"
  (check-type src-pane sxfiler/types/pane:pane)
  (check-type dest-pane sxfiler/types/pane:pane)

  (let ((src-dir (pane-directory src-pane))
        (dest-dir (pane-directory dest-pane))
        (focused-item (sxfiler/types/pane:find-focused-item src-pane)))
    (when focused-item
      (let ((filename (sxfiler/types/file-stat:file-stat-filename focused-item)))
        (copy-file (uiop:merge-pathnames* filename src-dir)
                   (uiop:merge-pathnames* filename dest-dir))))))

(defun move-file-to-another-pane (src-pane dest-pane)
  "move file focusing in SRC-PANE to DEST-PANE. This function has sidef effect."
  (check-type src-pane sxfiler/types/pane:pane)
  (check-type dest-pane sxfiler/types/pane:pane)

  (let ((src-dir (pane-directory src-pane))
        (dest-dir (pane-directory dest-pane))
        (focused-item (sxfiler/types/pane:find-focused-item src-pane)))
    (when focused-item
      (let ((filename (sxfiler/types/file-stat:file-stat-filename focused-item)))
        (move-file (uiop:merge-pathnames* filename src-dir)
                   (uiop:merge-pathnames* filename dest-dir))))))

(defun delete-focused-item (pane)
  "Delete a file current focused file in PANE."
  (check-type pane sxfiler/types/pane:pane)

  (let ((dir (pane-directory pane))
        (focused-item (sxfiler/types/pane:find-focused-item pane)))
    (when focused-item
      (let ((filename (sxfiler/types/file-stat:file-stat-filename focused-item))
            (dirname (sxfiler/types/file-stat:file-stat-directory focused-item)))
        (if (sxfiler/types/file-stat:file-stat-directory-p focused-item)
            (uiop:delete-directory-tree (uiop:merge-pathnames* "" dirname) :validate t)
            (uiop:delete-file-if-exists (uiop:merge-pathnames* filename dir)))))))

;; functions are related state. These functions should use from API
(defun copy-file-from-active-to-inactive (state)
  "copy file focusing on active pane of state to inactive pane of state."
  (check-type state sxfiler/state:state)
  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (copy-file-to-another-pane active-pane inactive-pane)
    (sxfiler/state:set-inactive-pane state (renew-file-list inactive-pane))))

(defun move-file-from-active-to-inactive (state)
  "Move file focising on active pane in state to inactive pane in state."
  (check-type state sxfiler/state:state)
  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (move-file-to-another-pane active-pane inactive-pane)
    (sxfiler/state:set-inactive-pane state (renew-file-list inactive-pane))))

(defun delete-focused-item-from-active (state)
  "Delete focusing item on active pane in state."
  (check-type state sxfiler/state:state)
  (multiple-value-bind (active-pane inactive-pane) (panes-of-state state)
    (declare (ignorable inactive-pane))
    (delete-focused-item active-pane)
    (sxfiler/state:update-active-pane state (renew-file-list active-pane))))

(defun expose (server)
  "expose functions for file-related operations to JSON-RPC"
  (jsonrpc:expose server "/file/delete"
                  #'(lambda (args)
                      (check-type args hash-table)
                      (let ((force-delete (gethash "force" args)))
                        (with-root-state (state)
                          (delete-focused-item-from-active state)))))
  (jsonrpc:expose server "/file/move"
                  #'(lambda (args)
                      (check-type args hash-table)
                      (let ((force-move (gethash "force" args)))
                        (with-root-state (state)
                          (move-file-from-active-to-inactive state)))))
  (jsonrpc:expose server "/file/copy"
                  #'(lambda (args)
                      (check-type args hash-table)
                      (let ((force-copy (gethash "force" args)))
                        (with-root-state (state)
                          (copy-file-from-active-to-inactive state))))))
