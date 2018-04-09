;;; main.lisp
(in-package :cl-user)
(defpackage :sxfiler/core
  (:use :common-lisp :clack)
  (:export :main))  ; 今回作成する関数(まだない)をパッケージ外部に公開
(in-package :sxfiler/core)

(defun main (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, Clack!")))
