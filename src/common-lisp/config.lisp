;;;; Configuration system for sxfiler
(in-package #:cl-user)
(defpackage #:sxfiler/config
  (:use #:cl)
  (:export #:key-maps
           #:make-key-maps
           #:config
           #:make-config
           #:config-key-maps
           #:parse-config-form
           #:load-from-file))
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

;;; functions to parse configuration file written by Common Lisp.

(defun key-map-p (form)
  (and (listp form)
       (not (null form))))

(defun check-components-input (key-map)
  "A partial test for configuration components."
  (unless (listp key-map)
    (error (format nil "The field key-map must be list"))))

(defun key-definition-p (key-def)
  "Predicates valid key definition."
  (and (alexandria:proper-list-p key-def)
       (not (null key-def))
       (<= 2 (length (alexandria:plist-alist key-def)))))

(defun extract-config-components (form)
  "Take a alist that contains each components in FORM."
  (check-type form list)
  (let ((key-maps (cdr (assoc :key-maps (cdr form)))))
    (list :key-maps key-maps)))

(defun parse-key-map (key-maps pane-type)
  "Read a key map bound PANE-TYPE in KEY-MAPS."
  (check-type key-maps list)
  (check-type pane-type symbol)
  (if (not (null key-maps))
      (let ((keyconfig (cadr (assoc pane-type key-maps)))
            (key-map (make-hash-table)))
        (dolist (key-def keyconfig)
          (when (key-definition-p key-def)
            (let* ((key-def (alexandria:plist-alist key-def))
                   (key (cdr (assoc :key key-def)))
                   (command (cdr (assoc :command key-def))))
              (setf (gethash key key-map) command))))
        key-map)
      (make-hash-table)))

(defun parse-key-maps (key-maps)
  "Make key maps from KEY-MAPS."
  (when (and key-maps
             (not (null key-maps)))
    (let ((file-list-map (parse-key-map key-maps :file-list)))
      (sxfiler/config:make-key-maps :file-list file-list-map))))

(defun parse-config-form (form)
  "Read form as configuration and get a configuration structure. "
  (let* ((component-plist (extract-config-components form))
         (component-alist (alexandria:plist-alist component-plist))
         (key-maps (cdr (assoc :key-maps component-alist))))
    (check-components-input key-maps)
    (let ((key-maps (parse-key-maps key-maps))
          (config (sxfiler/config:make-config)))
      (when key-maps
        (setf (sxfiler/config:config-key-maps config) key-maps))
      config)))

(defun load-from-file (file)
  "Read and parse configuration from FILE."
  (check-type file string)
  (let ((form (uiop:read-file-form file)))
    (parse-config-form form)))
