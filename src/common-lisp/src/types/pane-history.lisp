;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/src/types/pane-history
  (:nicknames #:sxfiler/types/pane-history)
  (:use #:cl)
  (:import-from #:yason)
  (:export #:pane-history
           #:make-pane-history
           ))
(in-package #:sxfiler/src/types/pane-history)

;; Item of history.
(defstruct history-item
  (directory "" :type string)
  (focused-item "" :type (or null string))
  (timestamp (get-universal-time) :type (unsigned-byte 64)))

(defun get-duration-seconds-from-utc (time)
  "Get duration of seconds from UTC timezone."
  (multiple-value-bind (second minute hour date month year daylight dst-p tz) (decode-universal-time time)
    (declare (ignore second))
    (declare (ignore minute))
    (declare (ignore hour))
    (declare (ignore date))
    (declare (ignore month))
    (declare (ignore year))
    (declare (ignore daylight))
    (declare (ignore dst-p))
    (* -1 tz 60 60)))

(defun format-utc-timestring (time)
  "Format universal time to date-time string on UTC timezone that the format is `yyyy-mm-ddTHH:MM:ss.000Z' for.
Notice Common Lisp's universal time has only second resolution, do not have millisecond.
"
  (let ((duration (get-duration-seconds-from-utc time)))
    (multiple-value-bind (second minute hour date month year daylight dst-p tz) (decode-universal-time (- time duration))
      (declare (ignore daylight))
      (declare (ignore dst-p))
      (declare (ignore tz))
      (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.000Z" year month date hour minute second))))

(defmethod yason:encode ((object history-item) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "directory" (history-item-directory object))
      (yason:encode-object-element "focusedItem" (history-item-focused-item object))
      (let ((utc (format-utc-timestring (history-item-timestamp object))))
        (yason:encode-object-element "timestamp" utc)))))

;; Histories of pane
(defstruct pane-history
  (records '() :type list)
  (max-records 100 :type number))

(defmethod yason:encode ((object pane-history) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "records" (pane-history-records object))
      (yason:encode-object-element "maxRecords" (pane-history-max-records object)))))
