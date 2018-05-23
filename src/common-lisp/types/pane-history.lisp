;;; Define structures and types
(in-package :cl-user)
(defpackage #:sxfiler/types/pane-history
  (:use #:cl)
  (:import-from #:sxfiler/types/pane)
  (:export #:pane-history
           #:make-pane-history
           #:pane-history-records
           #:pane-history-location
           #:record-directory
           #:record-focused-item
           #:record-timestamp

           #:push-record
           #:sort-history-by-timestamp
           ))
(in-package #:sxfiler/types/pane-history)

;; Item of history.
(defstruct record
  (directory "" :type string)
  (focused-item "" :type (or null string))
  (timestamp (get-universal-time) :type (unsigned-byte 64)))

(defun pane->record (pane)
  "Convert from PANE to `record'."
  (check-type pane sxfiler/types/pane:pane)
  (make-record :directory (sxfiler/types/pane:pane-directory pane)
               :focused-item (sxfiler/types/pane:pane-focused-item pane)))

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

(defmethod yason:encode ((object record) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "directory" (record-directory object))
      (yason:encode-object-element "focusedItem" (record-focused-item object))
      (let ((utc (format-utc-timestring (record-timestamp object))))
        (yason:encode-object-element "timestamp" utc)))))

;; Histories of pane
(defstruct pane-history
  (location :left :type (member :left :right))
  (records (list) :type list)
  (max-records 100 :type number))

(defmethod yason:encode ((object pane-history) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "location"
                                   (format nil "~(~A~)" (pane-history-location object)))
      (yason:with-object-element ("records")
        (yason:with-array ()
          (mapcar #'yason:encode-array-element (pane-history-records object))))
      (yason:encode-object-element "maxRecords" (pane-history-max-records object)))))

;; History mutations
(defun push-record (obj record)
  "Push a `record' tto head of records in`obj', or remove records having same place of `record' and add it to head of history.
This function will immutable, so return new structure of pane-history.
 "
  (check-type obj pane-history)
  (flet ((same-place-p (v) (string= (record-directory v) (record-directory record))))
    (let* ((new-records (cons record (remove-if #'same-place-p (pane-history-records obj))))
           (new-records (if (< (pane-history-max-records obj) (length new-records))
                            (reverse (cdr (reverse new-records)))
                            new-records)))
      (make-pane-history :records new-records
                         :max-records (pane-history-max-records obj)))))

(defun sort-history-by-timestamp (obj &key (direction :asc))
  "Get a new pane-history obj sorted records with `timestamp' with `predicate'"
  (check-type obj pane-history)
  (check-type direction symbol)
  (let ((records (copy-list (pane-history-records obj)))
        (predicate (case direction
                     (:asc #'<)
                     (:desc #'>)
                     (t #'<))))
    (make-pane-history :records (sort records (lambda (a b)
                                                (funcall predicate
                                                         (record-timestamp a)
                                                         (record-timestamp b))))
                       :max-records (pane-history-max-records obj))))
