(in-package #:cl-user)
(defpackage #:sxfiler/t/parse-config
  (:use #:cl #:rove #:sxfiler/parse-config #:sxfiler/config))
(in-package #:sxfiler/t/parse-config)

(defun config-json-equal (config1 config2)
  (let ((actual (make-string-output-stream))
        (expected (make-string-output-stream)))
    (yason:encode config1 actual)
    (yason:encode config2 expected)
    (string= (get-output-stream-string actual)
             (get-output-stream-string expected))))

(deftest parse-config-test
  (testing "Empty keymap if empty keymaps in config"
    (let ((config '(:config
                    (:key-maps (:file-list ())))))
      (ok (config-json-equal (parse-config-form config)
                             (make-config)))))

  (testing "Apply key/command pair in file-list config"
    (let ((config '(:config
                    (:key-maps
                     (:file-list
                      ((:key "a" :command "b")
                       ))))))
      (let ((actual (parse-config-form config))
            (expected (make-config
                       :key-maps (make-key-maps :file-list (alexandria:plist-hash-table
                                                            '("a" "b"))))))
        (ok (config-json-equal actual expected)))))
  )
