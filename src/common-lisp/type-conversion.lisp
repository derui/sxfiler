;;;; Provide conversion functions for types/* package.
(in-package :cl-user)
(defpackage #:sxfiler/type-conversion
  (:use #:cl)
  (:import-from #:sxfiler/types/pane
                #:pane
                #:pane-file-list)
  (:import-from #:sxfiler/types/file-stat
                #:file-stat-filename
                #:file-stat-directory)
  (:import-from #:sxfiler/types/pane-history
                #:pane-history
                #:record-directory
                #:pane-history-records
                #:sort-history-by-timestamp)
  (:export #:pane->candidates
           #:history->candidates))
(in-package #:sxfiler/type-conversion)

(defun pane->candidates (pane)
  "Convert pane to candidates used for completer."
  (check-type pane pane)

  (mapcar (lambda (stat)
            (let ((path (uiop:merge-pathnames* (file-stat-filename stat)
                                               (file-stat-directory stat))))
              (cons (namestring path) stat)))
          (pane-file-list pane)))

(defun history->candidates (history)
  "Convert HISTORY to candidates used for completer."
  (check-type history pane-history)

  (mapcar (lambda (record) (cons (record-directory record) record))
          (sort-history-by-timestamp (pane-history-records history))))
