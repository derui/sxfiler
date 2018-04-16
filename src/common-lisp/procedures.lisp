;;; Define procedures of JSON-RPC
(in-package :cl-user)
(defpackage #:sxfiler/procedures
  (:use #:cl #:jsonrpc)
  (:export #:expose-procedures))  ; 今回作成する関数(まだない)をパッケージ外部に公開
(in-package #:sxfiler/procedures)

(defun expose-procedures (server)
  (jsonrpc:expose server "sum" (lambda (args) (reduce #'+ args))))
