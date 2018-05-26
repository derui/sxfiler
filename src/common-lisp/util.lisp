;;; Define struct for single-source-of-truth state.
(in-package #:cl-user)
(defpackage #:sxfiler/util
  (:use #:cl)
  (:export #:add-nickname))
(in-package #:sxfiler/util)

(defun add-nickname (package nickname)
  "Add new nickname to package"
  (cond ((stringp package)
         (setf package (find-package (string-upcase package))))
        ((symbolp package)
         (setf package (find-package package))))
  (check-type package package)
  (check-type nickname (or symbol string))
  (rename-package package (package-name package)
                  (adjoin nickname
                          (package-nicknames package)
                          :test #'string=)))
