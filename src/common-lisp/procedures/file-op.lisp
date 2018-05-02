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
                #:pane-directory
                #:renew-file-list)
  (:import-from #:sxfiler/types/dialog-state)
  (:import-from #:sxfiler/dialogs/confirmation
                #:make-confirmation-behavior)
  (:import-from #:sxfiler/types/file-stat
                #:file-stat-equal
                #:file-stat-id)
  (:export #:expose))
(in-package #:sxfiler/procedures/file-op)

(defmacro with-focused-item ((src-pane) &body body)
  "Wrap operations to use focused item in src-pane."
  (if (= 0 (length body))
      '()
      (alexandria:with-gensyms (file-list focused-item find-file)
        `(let ((,file-list (sxfiler/types/pane:pane-file-list ,src-pane))
               (,focused-item (sxfiler/types/pane:pane-focused-item ,src-pane)))
           (flet ((,find-file (item)
                    (when item
                      (find-if (lambda (v) (file-stat-equal item v)) ,file-list))))

             (if (funcall #',find-file ,focused-item)
                 ,@body
                 (error (make-condition 'not-found-file-id-error :id (file-stat-id ,focused-item)))))))))

;; Conditions
(define-condition not-found-file-id-error (error)
  ((id :initarg :id
       :reader not-found-file-id-error-id))
  (:documentation "Raise when not found any file ID")
  (:report (lambda (condition stream)
             (format stream "Not found File ID: ~A" (not-found-file-id-error-id condition))))))

;; Primitive functions are used with pathname.
(defun copy-file (src dest)
  "simply coping file from SRC to DIST.
This function has side effect."
  (check-type src pathname)
  (check-type dest pathname)

  (uiop:copy-file (namestring src)
                  (namestring dest)))

(defun move-file (src dest &key (overwrite t))
  "simply move file from SRC to DIST. This function has side effect."
  (check-type src pathname)
  (check-type dest pathname)

  (uiop:rename-file-overwriting-target src dest))

;; functions are related current focusing item.
(defun copy-file-to-another-pane (src-pane dest-pane)
  "copy file focusing on SRC-PANE to DEST-PANE. this function has side effect"
  (check-type src-pane sxfiler/types/pane:pane)
  (check-type dest-pane sxfiler/types/pane:pane)

  (with-focused-item (src-pane)
    (let ((src-dir (pane-directory src-pane))
          (dest-dir (pane-directory dest-pane))
          (focused-item (sxfiler/types/pane:pane-focused-item src-pane)))
      (let ((filename (sxfiler/types/file-stat:file-stat-filename focused-item)))
        (copy-file (uiop:merge-pathnames* filename src-dir)
                   (uiop:merge-pathnames* filename dest-dir))))))

(defun move-file-to-another-pane (src-pane dest-pane)
  "move file focusing in SRC-PANE to DEST-PANE. This function has sidef effect."
  (check-type src-pane sxfiler/types/pane:pane)
  (check-type dest-pane sxfiler/types/pane:pane)

  (with-focused-item (src-pane)
    (let ((src-dir (pane-directory src-pane))
          (dest-dir (pane-directory dest-pane))
          (focused-item (sxfiler/types/pane:pane-focused-item src-pane)))
      (let ((filename (sxfiler/types/file-stat:file-stat-filename focused-item)))
        (move-file (uiop:merge-pathnames* filename src-dir)
                   (uiop:merge-pathnames* filename dest-dir))))))

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

;; utility to open some dialog.
(defun open-confirm-dialog (state handle)
  "Open dialog to ask confirmation to copy file.
User must call '/dialog/confirmation' method after this.
"
  (check-type state sxfiler/state:state)
  (check-type handle function)

  (setf (sxfiler/state:state-dialog-state state)
        (sxfiler/types/dialog-state:open-dialog (sxfiler/state:state-dialog-state state)
                                                (make-confirmation-behavior :handle handle))))

(defun expose (server)
  "expose functions for file-related operations to JSON-RPC"
  (jsonrpc:expose server "/file/movefile"
                  (lambda (args)
                    (check-type args hash-table)
                    (let ((force-move (gethash "force" args)))
                      (with-root-state (state)
                        (if force-move
                            (move-file-from-active-to-inactive state)
                            (open-confirm-dialog state (lambda ()
                                                         (with-root-state (state)
                                                           (move-file-from-active-to-inactive state)))))))))
  (jsonrpc:expose server "/file/copyFile"
                  (lambda (args)
                    (check-type args hash-table)
                    (let ((force-copy (gethash "force" args)))
                      (with-root-state (state)
                        (if force-copy
                            (copy-file-from-active-to-inactive state)
                            (open-confirm-dialog state (lambda ()
                                                         (with-root-state (state)
                                                           (copy-file-from-active-to-inactive state))))))))))
