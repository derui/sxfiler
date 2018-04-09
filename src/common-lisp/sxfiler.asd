;;; sxfiler.asd
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
