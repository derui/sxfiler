;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types
  (:use #:cl)
  (:import-from #:yason)
  (:export #:make-pane
           #:make-file-stat))
(in-package #:sxfiler/types)

;; pane: contains all state of pane without visual information

(defstruct pane
  (directory "" :type string)
  (file-list (list) :type list)
  (focused-item "" :type (or null string))
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

(defstruct file-stat
  (id "" :type string)
  (filename "" :type string)
  (directory "" :type string)
  (link-path nil :type string))
