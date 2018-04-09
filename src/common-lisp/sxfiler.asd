;;; sxfiler.asd
(in-package :cl-user)    ; どのパッケージにいるかわからないのでCL-USERパッケージにする
(defpackage :sxfiler-asd  ; ASDFのシステム定義用のパッケージをつくる
  (:use :cl :asdf))      ; 標準関数とASDFの関数をパッケージ修飾なしで呼べるようにする
(in-package :sxfiler-asd) ; 作ったパッケージにする

(defsystem :sxfiler
    ;; システム定義のスタイルをpackage-inferred-systemにする
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :description "Implementation of sxfiler main server on CommonLisp"
    :version "0.1"
    :author "derui"
    :license "MIT"
    :depends-on (:sxfiler/core
                 :clack
                 :websocket-driver))
