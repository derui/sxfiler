
(asdf:defsystem "sxfiler/t"
  ;; システム定義のスタイルをpackage-inferred-systemにする
  :class :package-inferred-system
  :description "Test for sxfiler main server"
  :version "0.1"
  :author "derui"
  :license "MIT"
  :depends-on (:rove
               "sxfiler/t/types")
  :perform (asdf:test-op (o c) (uiop:symbol-call :rove ':run c)))
