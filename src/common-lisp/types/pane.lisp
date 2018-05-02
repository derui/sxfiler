;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types/pane
  (:nicknames #:sxfiler/types/pane)
  (:use #:cl)
  (:import-from #:sxfiler/types/file-stat :get-file-stat)
  (:export #:pane
           #:make-pane
           #:pane-directory
           #:pane-file-list
           #:pane-focused-item
           #:pane-marked-item

           #:renew-file-list))
(in-package #:sxfiler/types/pane)

;; pane: contains all state of pane without visual information

(defstruct pane
  (directory "" :type string)
  (file-list (list) :type list)
  (focused-item nil :type (or null sxfiler/types/file-stat:file-stat))
  (marked-item (list) :type list))

;; encoder for pane. The pane will encode json as below
#|
{
"directory": ``(pane-directory pane)'',
"fileList": array of file-list and encoded file stat,
"focusedItem": [empty, ""] or [focused, focused item id],
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
