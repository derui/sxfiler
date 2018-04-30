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
  (link-path "" :type string)
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
                 (let* ((digest (ironclad:digest-sequence :sha1 (sb-ext:string-to-octets name)))
                        (str-list (map 'list (lambda (v)
                                               (format nil "~2,,,'0@a" (write-to-string v :base 16)))
                                       digest)))
                   (apply #'concatenate 'string str-list))))
          (make-file-stat
           :id (make-id (namestring path))
           :filename (file-namestring path)
           :directory (directory-namestring path)
           :uid (cdr (assoc :uid stats))
           :gid (cdr (assoc :gid stats))
           :mode (cdr (assoc :mode stats))
           :atime (cdr (assoc :atime stats))
           :mtime (cdr (assoc :mtime stats))
           :ctime (cdr (assoc :ctime stats))
           :size (cdr (assoc :size stats))))))))

(defun enc-stat (obj key-funcs)
  (dolist (key-func key-funcs)
    (yason:encode-object-element (car key-func) (funcall (cdr key-func) obj))))

(defmethod yason:encode ((object file-stat) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (enc-stat object '(("id" . file-stat-id)
                         ("filename" . file-stat-filename)
                         ("directory" . file-stat-directory)
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
