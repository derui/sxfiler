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
               :alexandria
               :sxfiler/main)
  :in-order-to ((asdf:test-op (asdf:test-op :sxfiler/t))))

(asdf:defsystem :sxfiler/t
  ;; システム定義のスタイルをpackage-inferred-systemにする
  :class :package-inferred-system
  :description "Test for sxfiler main server"
  :version "0.1"
  :author "derui"
  :license "MIT"
  :depends-on (:rove
               :sxfiler/t/parse-config
               :sxfiler/t/types/pane)
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :rove '#:run c)))
