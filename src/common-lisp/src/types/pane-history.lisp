;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/src/types/pane-history
  (:nicknames #:sxfiler/types/pane-history)
  (:use #:cl)
  (:import-from #:yason)
  (:export #:pane-history
           #:make-pane-history
           #:pane-history-directory
           #:pane-history-focused-item
           #:pane-history-timestamp
           ))
(in-package #:sxfiler/src/types/pane-history)

(defstruct pane-history
  (directory "" :type string)
  (focused-item "" :type (or null string))
  (timestamp (get-universal-time) :type (unsigned-byte 64)))

(defun get-utc-timestring (time)
  (multiple-value-bind (second minute hour date month year daylight dst-p tz) (decode-universal-time time)
    (declare (ignore daylight))
    (declare (ignore dst-p))
    (declare (ignore tz))
    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~3,'0f" year month date hour minute second 0)))

(defmethod yason:encode ((object pane-history) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "directory" (pane-history-directory object))
      (yason:encode-object-element "focusedItem" (pane-history-focused-item object))
      (yason:encode-object-element "timestamp" (pane-history-timestamp object))
      )))
