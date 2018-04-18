;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:uiop)
  (:import-from #:ironclad)
  (:export #:make-pane

           ;; file stat
           #:get-file-stat
           #:file-stat-directory-p
           #:file-stat-file-p
           #:file-stat-symlink-p))
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

;; stat of file.
(defstruct file-stat
  (id "" :type string)
  (filename "" :type string)
  (directory "" :type string)
  (link-path "" :type string)
  (mode 0 :type (unsigned-byte 32))
  (uid 0 :type (unsigned-byte 16))
  (gid 0 :type (unsigned-byte 16))
  (atime 0 :type (unsigned-byte 64))
  (ctime 0 :type (unsigned-byte 64))
  (mtime 0 :type (unsigned-byte 64))
  (size 0 :type (unsigned-byte 64)))

(defun file-stat-directory-p (obj)
  (let ((mode (file-stat-mode obj)))
    (< 0 (logand mode #o040000))))

(defun file-stat-symlink-p (obj)
  (let ((mode (file-stat-mode obj)))
    (< 0 (logand mode #o120000))))

(defun file-stat-file-p (obj)
  (let ((mode (file-stat-mode obj)))
    (< 0 (logand mode #o100000))))

;; function to probe file stat.
(defun unix-stat-to-assoc (stat-list)
  `((:dev . ,(nth 1 stat-list))
    (:ino . ,(nth 2 stat-list))
    (:mode . ,(nth 3 stat-list))
    (:nlink . ,(nth 4 stat-list))
    (:uid . ,(nth 5 stat-list))
    (:gid . ,(nth 6 stat-list))
    (:rdev . ,(nth 7 stat-list))
    (:size . ,(nth 8 stat-list))
    (:atime . ,(nth 9 stat-list))
    (:mtime . ,(nth 10 stat-list))
    (:ctime . ,(nth 11 stat-list))
    (:blksize . ,(nth 12 stat-list))
    (:blocks . ,(nth 13 stat-list))))

(defun get-file-stat (path)
  (let ((path (uiop:probe-file* path :truename t)))
    (when path
      (let* ((stats (multiple-value-list (sb-unix:unix-lstat (namestring path))))
             (stats (unix-stat-to-assoc stats)))
        (flet ((make-id (name)
                 (let* ((digest (ironclad:digest-sequence :sha1 (string-to-octets name)))
                        (str-list (map 'list (lambda (v)
                                               (format nil "~2,,,'0@a" (write-to-string v :base 16)))
                                       digest)))
                   (apply #'concatenate 'string str-list))))
          (make-file-stat
           :id (make-id (file-namestring path))
           :filename (file-namestring path)
           :directory (directory-namestring path)
           :uid (cdr (assoc :uid stats))
           :gid (cdr (assoc :gid stats))
           :mode (cdr (assoc :mode stats))
           :atime (cdr (assoc :atime stats))
           :mtime (cdr (assoc :mtime stats))
           :ctime (cdr (assoc :ctime stats))
           :size (cdr (assoc :size stats))))))))
