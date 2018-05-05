;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types/pane
  (:nicknames #:sxfiler/types/pane)
  (:use #:cl)
  (:import-from #:sxfiler/types/file-stat
                #:file-stat-id
                #:file-stat-equal
                #:get-file-stat)
  (:import-from #:alexandria
                #:if-let)
  (:export #:pane
           #:make-pane
           #:pane-directory
           #:pane-file-list
           #:pane-focused-item
           #:pane-marked-item

           #:renew-file-list
           #:toggle-mark
           #:find-focused-item
           #:focus-item))
(in-package #:sxfiler/types/pane)

;; pane: contains all state of pane without visual information

(defstruct pane
  (directory "" :type string)
  (file-list (list) :type list)
  (focused-item nil :type (or null string))
  (marked-item (list) :type list))

;; encoder for pane. The pane will encode json as below
#|
{
"directory": ``(pane-directory pane)'',
"fileList": array of file-list and encoded file stat,
"focusedItem": ID of file-stat if item focusing
"markedItem": array of marked-item, marked-item will contain file id
}
|#
(defmethod yason:encode ((object pane) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "directory" (pane-directory object))
      (yason:with-object-element ("fileList")
        (yason:with-array ()
          (mapcar #'yason:encode-array-element (pane-file-list object))))
      (yason:encode-object-element "focusedItem"
                                   (let ((v (pane-focused-item object)))
                                     (cond
                                       ((null v) nil)
                                       (t v))))
      (yason:with-object-element ("markedItems")
        (yason:with-array ()
          (mapcar #'yason:encode-array-element (pane-marked-item object)))))))

;; pane related functions
(defun files-in-directory (directory)
  "Get list of file stat in the `directory'.
Return nil if directory do not found or is not directory"
  (if (and (uiop:probe-file* directory :truename t)
           (uiop:directory-exists-p directory))
      (concatenate 'list
                   (mapcar #'get-file-stat (uiop:directory-files directory))
                   (mapcar #'get-file-stat (uiop:subdirectories directory)))
      '()))

(defun renew-file-list (obj &key (directory nil))
  "Get file-list renewaled pane object."
  (check-type obj pane)
  (let ((copied-pane (copy-structure obj))
        (dir (if directory directory (pane-directory obj))))
    (when (uiop:probe-file* dir :truename t)
      (setf (pane-directory copied-pane) (namestring (uiop:probe-file* dir :truename t))))

    (setf (pane-file-list copied-pane) (files-in-directory dir))
    (setf (pane-marked-item copied-pane) (copy-list (pane-marked-item obj)))
    copied-pane))

(defun toggle-mark (pane id)
  "Toggle mark of PANE specified item by ID."
  (check-type pane pane)
  (check-type id string)
  (let ((file-list (pane-file-list pane))
        (copied (copy-structure pane)))
    (if (member-if (lambda (v) (string= id (sxfiler/types/file-stat:file-stat-id v)))
                   file-list)
        (progn
          (setf (pane-marked-item copied) (if (member id (pane-marked-item copied) :test #'string=)
                                              (remove id (pane-marked-item copied) :test #'string=)
                                              (cons id (pane-marked-item copied))))
          copied)
        copied)))

(defun find-focused-item (pane)
  "Find focused item from file-list of PANE."
  (check-type pane pane)
  (let ((focused (pane-focused-item pane)))
    (find-if #'(lambda (v) (string= (file-stat-id v) focused))
             (pane-file-list pane))))

(defun focus-item (pane item)
  "Focus ITEM if file list of PANE contains it.
This function returns new pane structure if found ITEM."
  (check-type pane pane)
  (check-type item sxfiler/types/file-stat:file-stat)
  (if-let ((item (find-if #'(lambda (v) (file-stat-equal v item))
                          (pane-file-list pane))))
    (let ((pane (copy-structure pane)))
      (setf (pane-focused-item pane) (file-stat-id item))
      pane)
    pane))
