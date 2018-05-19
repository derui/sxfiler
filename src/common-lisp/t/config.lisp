(in-package #:cl-user)
(defpackage #:sxfiler/t/config
  (:use #:cl #:rove #:sxfiler/config))
(in-package #:sxfiler/t/config)

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

  (testing "Apply key/command pairs in key map"
    (let ((config '(:config
                    (:key-maps
                     (:file-list
                      ((:key "a" :command "b")
                       (:key "b" :command "c")
                       ))))))
      (let ((actual (parse-config-form config))
            (expected (make-config
                       :key-maps (make-key-maps :file-list (alexandria:plist-hash-table
                                                            '("a" "b" "b" "c"))))))
        (ok (config-json-equal actual expected)))))
  )

(defparameter *tmp-directory* "./tmp-parse-config/")

(setup
  (ensure-directories-exist *tmp-directory* :verbose t :mode #o755)
  (with-open-file (stream (uiop:merge-pathnames* "empty.lisp" *tmp-directory* )
                          :direction :output :if-exists :supersede)
    (write-string "(:config)" stream))
  (with-open-file (stream (uiop:merge-pathnames* "single-key.lisp" *tmp-directory*)
                          :direction :output
                          :if-exists :supersede)
    (write-string "(:config (:key-maps (:file-list ((:key \"a\" :command \"b\")))))"
                  stream)))

(teardown
  (uiop:delete-directory-tree (uiop:truename* *tmp-directory*) :validate t :if-does-not-exist :ignore))

(deftest load-config-test
  (testing "Empty configuration if toplevel configuration only"
    (let ((path (uiop:ensure-absolute-pathname "empty.lisp"
                                               (uiop:truename* *tmp-directory*))))
      (let ((config (load-from-file (namestring path))))
        (let ((expected (make-config)))
          (ok (config-json-equal config expected))))))

  (testing "Having key map if configuration file have it"
    (let ((path (uiop:ensure-absolute-pathname "single-key.lisp"
                                               (uiop:truename* *tmp-directory*))))
      (let ((config (load-from-file (namestring path))))
        (let ((expected (make-config
                         :key-maps (make-key-maps :file-list (alexandria:plist-hash-table
                                                              '("a" "b"))))))
          (ok (config-json-equal config expected)))))))
