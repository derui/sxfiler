(in-package #:cl-user)
(defpackage #:sxfiler/t/types
  (:use #:cl #:rove)
  (:import-from :yason)
  (:import-from :sxfiler/types))
(in-package #:sxfiler/t/types)

(deftest parse-message-test
  (testing "pane default convert"
    (let ((s (make-string-output-stream)))
      (yason:encode (sxfiler/types:make-pane) s)
      (ok (string-equal "{\"directory\":\"\",\"fileList\":[],\"focusedItem\":\"\",\"markedItems\":[]}"
                        (get-output-stream-string s)) "default json")))
  (testing "pane's focusedItem as null if focused item do not have"
    (let ((s (make-string-output-stream))
          (pane (sxfiler/types:make-pane :focused-item nil)))
      (yason:encode pane s)
      (ok (string-equal "{\"directory\":\"\",\"fileList\":[],\"focusedItem\":null,\"markedItems\":[]}"
                        (get-output-stream-string s)) "no any focusing"))))
