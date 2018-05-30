;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types/pane
  (:nicknames #:sxfiler/types/pane)
  (:use #:cl)
  (:import-from #:sxfiler/types/file-stat
                #:file-stat-id
                #:file-stat-equal
                #:file-stat-directory
                #:file-stat-filename
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
           #:move-focus
           #:make-directory))
(in-package #:sxfiler/types/pane)

;; pane: contains all state of pane without visual information

(defstruct pane
  (location :left :type (member :left :right))
  (sort-mode :name :type (member :name :date))
  (sort-order :asc :type (member :asc :desc))
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
      (yason:encode-object-element "location"
                                   (format nil "~(~A~)" (pane-location object)))
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

(defun sort-file-list (file-list &key (mode :name) (order :asc))
  "Sort the file list by MODE. This function does not mutate original FILE-LIST"
  (check-type file-list list)
  (check-type mode (member :name :date))
  (check-type order (member :asc :desc))

  (let ((copied-list (copy-list file-list))
        (key (ecase mode
               (:name #'(lambda (v)
                          (uiop:native-namestring (uiop:subpathname* (sxfiler/types/file-stat:file-stat-directory v)
                                                                     (sxfiler/types/file-stat:file-stat-filename v)))))
               (:date #'sxfiler/types/file-stat:file-stat-mtime)))
        (orderp (ecase order
                  (:asc (ecase mode
                          (:name #'string<)
                          (:date #'<)))
                  (:desc (ecase mode
                           (:name #'string>)
                           (:date #'>))))))
    (sort copied-list orderp :key key)))

(defun renew-file-list (obj &key (directory nil))
  "Get file-list renewaled pane object."
  (check-type obj pane)
  (let ((copied-pane (copy-structure obj))
        (dir (if directory directory (pane-directory obj))))
    (when (uiop:probe-file* dir :truename t)
      (setf (pane-directory copied-pane) (uiop:native-namestring (uiop:probe-file* dir :truename t))))
    (setf (pane-file-list copied-pane)
          (sort-file-list (files-in-directory dir)
                          :mode (pane-sort-mode copied-pane)
                          :order (pane-sort-order copied-pane)))
    (setf (pane-marked-item copied-pane) (copy-list (pane-marked-item obj)))
    (setf (pane-focused-item copied-pane)
          (when (first (pane-file-list copied-pane))
            (file-stat-id (first (pane-file-list copied-pane)))))
    copied-pane))

(defun toggle-mark (pane id)
  "Toggle mark of PANE specified item by ID."
  (check-type pane pane)
  (check-type id string)
  (let ((file-list (pane-file-list pane))
        (copied (copy-structure pane)))
    (if (member-if #'(lambda (v) (string= id (file-stat-id v)))
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

(defun move-focus (pane amount)
  "Move focus on file list of PANE with AMOUNT. If AMOUNT is negative,
move focus backword, or move it forward if AMOUNT is positive.

Return new `pane' structure focusd item updated.
 "
  (check-type pane pane)
  (check-type amount integer)
  (let ((focused (pane-focused-item pane))
        (file-list (pane-file-list pane)))
    (if-let ((pos (position-if #'(lambda (v) (string= (file-stat-id v) focused))
                               file-list)))
      (let ((next-pos (max 0 (min (1- (length file-list)) (+ pos amount)))))
        (let ((item (elt file-list next-pos))
              (new-pane (copy-structure pane)))
          (setf (pane-focused-item new-pane) (file-stat-id item))
          new-pane))
      pane)))

(defun make-directory (pane name)
  "Make the directory with the NAME if not exists. Always return applied RENEW-FILE-LIST
to PANE.
"
  (check-type pane pane)
  (check-type name string)
  (let ((dirname (uiop:ensure-directory-pathname (uiop:merge-pathnames* name (pane-directory pane)))))
    (unless (uiop:probe-file* dirname)
      (ensure-directories-exist dirname :mode #o755))
    (renew-file-list pane)))
