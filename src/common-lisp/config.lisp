;;;; Configuration system for sxfiler
(in-package #:cl-user)
(defpackage #:sxfiler/config
  (:use #:cl)
  (:export #:key-maps
           #:make-key-maps
           #:config
           #:make-config
           #:load-from-stream))
(in-package #:sxfiler/config)

(defstruct key-maps
  "Key map for renderer. Each key maps are `hash-table', key of it is key sequence,
and value of key is name of function.
"
  (file-list (make-hash-table) :type hash-table))

(defmethod yason:encode ((object key-maps) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "fileList" (key-maps-file-list object)))))

(defstruct config
  "All configurations."
  (key-maps (make-key-maps) :type key-maps))

(defmethod yason:encode ((object config) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "keyMaps" (config-key-maps object)))))

(defun load-from-stream (stream)
  "Load configuration structure from stream."
  (check-type stream stream)

  (let ((parsed (yason:parse stream))
        (config (make-config)))
    (setf (key-maps-file-list (config-key-maps config))
          (or (gethash "file_list" (or (gethash "key_map" parsed)
                                       (make-hash-table)))
              (make-hash-table)))
    config))
