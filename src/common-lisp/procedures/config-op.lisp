;;; exposable functions for operations of pane.
(in-package :cl-user)
(defpackage #:sxfiler/procedures/config-op
  (:use #:cl)
  (:import-from #:sxfiler/config)
  (:export #:expose

           #:*config*))
(in-package #:sxfiler/procedures/config-op)

(defparameter *config* (sxfiler/config:make-config)
  "Global configuration.")

(defparameter *config-lock* (sb-thread:make-mutex)
  "Mutex for lock configuration stored at *config*.")

(defmacro with-config ((var) &body body)
  `(if (and (sb-thread:holding-mutex-p *config-lock*)
            (eql sb-thread:*current-thread* (sb-thread:mutex-owner *config-lock*)))
       (let ((,var *config*))
         (progn
           ,@body))
       (sb-thread:with-mutex (*config-lock*)
         (let ((,var *config*))
           (progn
             ,@body)))))

(defun expose (server)
  (check-type server jsonrpc:server)
  (jsonrpc:expose server "config/current" #'(lambda (args)
                                              (declare (ignorable args))
                                              (with-config (config)
                                                config)))
  (jsonrpc:expose server "config/load" #'(lambda (args)
                                           (declare (ignorable args))
                                           (with-config (config)
                                             (setf *config* (sxfiler/config:load-from-file "default-config.lisp"))
                                             *config*))))
