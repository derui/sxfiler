;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types/file-stat
  (:use #:cl)
  (:export #:file-stat
           #:file-stat-id
           #:file-stat-filename
           #:file-stat-directory
           #:file-stat-link-path
           #:file-stat-mode
           #:file-stat-uid
           #:file-stat-gid
           #:file-stat-atime
           #:file-stat-ctime
           #:file-stat-mtime
           #:file-stat-size
           #:file-stat-directory-p
           #:file-stat-file-p
           #:file-stat-symlink-p

           #:file-stat-equal

           #:get-file-stat))
(in-package #:sxfiler/types/file-stat)

;; stat of file.
(defstruct file-stat
  (id "" :type string)
  (filename "" :type string)
  (directory "" :type string)
  (link-path "" :type (or null string))
  (mode 0 :type (unsigned-byte 32))
  (uid 0 :type (unsigned-byte 16))
  (gid 0 :type (unsigned-byte 16))
  (atime 0 :type (unsigned-byte 64))
  (ctime 0 :type (unsigned-byte 64))
  (mtime 0 :type (unsigned-byte 64))
  (size 0 :type (unsigned-byte 64)))

(defun file-stat-equal (p1 p2)
  "Return same or not between P1 and P2.
Only compare file id. Ignore any other properties in file-stat such as
size of item, atime, ctime, and mtime.
"
  (check-type p1 file-stat)
  (check-type p2 file-stat)
  (string= (file-stat-id p1) (file-stat-id p2)))

(defun mode-directory-p (mode)
  (< 0 (logand mode #o040000)))

(defun mode-symlink-p (mode)
  (< 0 (logand mode #o120000)))

(defun mode-file-p (mode)
  (< 0 (logand mode #o100000)))

(defun file-stat-directory-p (obj)
  (mode-directory-p (file-stat-mode obj)))

(defun file-stat-symlink-p (obj)
  (mode-symlink-p (file-stat-mode obj)))

(defun file-stat-file-p (obj)
  (mode-file-p (file-stat-mode obj)))

;; function to probe file stat.
(defun get-file-stat (path)
  (let* ((path (pathname                ; this combination to clear escape in path
                (uiop:native-namestring (uiop:probe-file* path :truename t)))))
    (when path
      (multiple-value-bind (result dev ino mode nlink uid gid rdev size atime mtime ctime blksize blocks)
          (sb-unix:unix-lstat (namestring path))
        (declare (ignorable result))
        (declare (ignorable rdev))
        (declare (ignorable dev))
        (declare (ignorable ino))
        (declare (ignorable nlink))
        (declare (ignorable blksize))
        (declare (ignorable blocks))
        (flet ((make-id (name)
                 (let* ((digest (ironclad:digest-sequence :sha1 (sb-ext:string-to-octets name)))
                        (str-list (map 'list
                                       (lambda (v)
                                         (format nil "~2,,,'0@a" (write-to-string v :base 16)))
                                       digest)))
                   (apply #'concatenate 'string str-list)))
               (get-original-if-link (path)
                 (when (mode-symlink-p mode)
                   (multiple-value-bind (path err)
                       (sb-unix:unix-readlink (namestring path))
                     (declare (ignorable err))
                     path))))
          (make-file-stat
           :id (make-id (namestring path))
           :filename (file-namestring path)
           :directory (directory-namestring path)
           :link-path (get-original-if-link path)
           :uid uid
           :gid gid
           :mode mode
           :atime atime
           :mtime mtime
           :ctime ctime
           :size size))))))

(defun enc-stat (obj key-funcs)
  (dolist (key-func key-funcs)
    (yason:encode-object-element (car key-func) (funcall (cdr key-func) obj))))

(defmethod yason:encode ((object file-stat) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (enc-stat object '(("id" . file-stat-id)
                         ("fileName" . file-stat-filename)
                         ("directory" . file-stat-directory)
                         ("linkPath" . file-stat-link-path)
                         ("uid" . file-stat-uid)
                         ("gid" . file-stat-gid)
                         ("mode" . file-stat-mode)
                         ("atime" . file-stat-atime)
                         ("mtime" . file-stat-mtime)
                         ("ctime" . file-stat-ctime)
                         ("size" . file-stat-size)
                         ("isDirectory" . file-stat-directory-p)
                         ("isFile" . file-stat-file-p)
                         ("isSymlink" . file-stat-symlink-p))))))
