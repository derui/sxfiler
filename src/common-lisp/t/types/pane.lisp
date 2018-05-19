(in-package #:cl-user)
(defpackage #:sxfiler/t/types/pane
  (:use #:cl #:rove)
  (:import-from :yason)
  (:import-from :sxfiler/types/pane))
(in-package #:sxfiler/t/types/pane)

(deftest parse-message-test
  (testing "pane default convert"
    (let ((s (make-string-output-stream)))
      (yason:encode (sxfiler/types/pane:make-pane) s)
      (ok (string-equal "{\"location\":\"left\",\"directory\":\"\",\"fileList\":[],\"focusedItem\":null,\"markedItems\":[]}"
                        (get-output-stream-string s))
          "default json")))

  (testing "pane's focusedItem as null if focused item do not have"
    (let ((s (make-string-output-stream))
          (pane (sxfiler/types/pane:make-pane :focused-item nil)))
      (yason:encode pane s)
      (ok (string-equal "{\"location\":\"left\",\"directory\":\"\",\"fileList\":[],\"focusedItem\":null,\"markedItems\":[]}"
                        (get-output-stream-string s))
          "no any focusing"))))
