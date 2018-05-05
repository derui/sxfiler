;;; exposable functions for JSON-RPC
(in-package :cl-user)
(defpackage #:sxfiler/procedures/completer-op
  (:use #:cl)
  (:import-from #:sxfiler/types/file-stat
                #:file-stat-directory
                #:file-stat-filename)
  (:import-from #:sxfiler/types/pane
                #:pane-file-list)
  (:import-from #:sxfiler/types/pane-history
                #:pane-history-records
                #:record-directory)
  (:import-from #:sxfiler/completer
                #:completer
                #:make-completer
                #:complete
                #:select-next-matched
                #:select-prev-matched)
  (:import-from #:sxfiler/state
                #:with-root-state
                #:state-active-pane
                #:state-active-pane-history)
  (:export #:expose))
(in-package #:sxfiler/procedures/completer-op)

(defparameter *completer* (make-completer))

(defparameter *completer-lock* (sb-thread:make-mutex))

(defmacro with-lock (() &body body)
  `(if (and (sb-thread:holding-mutex-p *completer-lock*)
            (eql sb-thread:*current-thread* (sb-thread:mutex-owner *completer-lock*)))
       (progn
         ,@body)
       (sb-thread:with-mutex (*completer-lock*)
         (progn
           ,@body))))

(defun initialize (completer state source)
  "Initialize completer with SOURCE.
SOURCE should be one of string below:

\"file-list\"
\"history\"

Active pane will be used for SOURCE.
"
  (check-type completer completer)
  (check-type state sxfiler/state:state)
  (check-type source string)
  (flet ((file-list->candidates (pane)
           (mapcar #'(lambda (v)
                       (if (sxfiler/types/file-stat:file-stat-directory-p v)
                           (let* ((dir (pathname (file-stat-directory v)))
                                  (last-dir-name (first (last (cdr (pathname-directory dir))))))
                             (cons last-dir-name (file-stat-directory v)))
                           (cons (file-stat-filename v)
                                 (namestring (uiop:merge-pathnames* (file-stat-filename v)
                                                                    (file-stat-directory v))))))
                   (pane-file-list pane)))
         (history->candidates (history)
           (mapcar #'(lambda (v) (cons (record-directory v) (record-directory v)))
                   (pane-history-records history))))
    (let ((candidates (cond ((string= "file-list" source)
                             (file-list->candidates (state-active-pane state)))
                            ((string= "history" source)
                             (history->candidates (state-active-pane-history state))))))
      (sxfiler/completer:update-candidates completer candidates))))

(defun expose (server)
  "Expose functions to JSON-RPC server"
  (jsonrpc:expose server
                  "completer/initialize"
                  (lambda (args)
                    (check-type args list)
                    (unless (<= 1 (length args))
                      (error (make-condition 'jsonrpc:jsonrpc-invalid-params
                                             :message "Must contain least one element")))
                    (with-root-state (state)
                      (setf *completer* (initialize *completer* state (first args))))))
  (jsonrpc:expose server
                  "completer/match"
                  (lambda (args)
                    (check-type args list)
                    (unless (<= 1 (length args))
                      (error (make-condition 'jsonrpc:jsonrpc-invalid-params
                                             :message "Must contain least one element")))
                    (with-lock ()
                      (setf *completer* (complete *completer* (first args))))))
  (jsonrpc:expose server
                  "completer/selectNextMatched"
                  (lambda (args)
                    (declare (ignorable args))
                    (with-lock ()
                      (setf *completer* (select-next-matched *completer*)))))
  (jsonrpc:expose server
                  "completer/selectPrevMatched"
                  (lambda (args)
                    (declare (ignorable args))
                    (with-lock ()
                      (setf *completer* (select-prev-matched *completer*))))))
