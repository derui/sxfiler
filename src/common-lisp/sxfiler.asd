;;; sxfiler.asd
(asdf:defsystem :sxfiler
  ;; システム定義のスタイルをpackage-inferred-systemにする
  :class :package-inferred-system
  :description "Implementation of sxfiler main server on CommonLisp"
  :version "0.1"
  :author "derui"
  :license "MIT"
  :depends-on (:jsonrpc
               :ironclad
               :yason
               :uiop
               :cl-syntax
               :cl-ppcre
               :sxfiler/main)
  :in-order-to ((asdf:test-op (asdf:test-op :sxfiler-test))))
