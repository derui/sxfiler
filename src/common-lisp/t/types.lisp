(in-package #:cl-user)
(defpackage #:sxfiler/t/types
  (:use #:cl #:rove))
(in-package #:sxfiler/t/types)

(deftest parse-message-test
  (testing "invalid message"
    (ok t "Parse error")))
