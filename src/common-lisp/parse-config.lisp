;;;; Configuration system for sxfiler
(in-package #:cl-user)
(defpackage #:sxfiler/parse-config
  (:use #:cl)
  (:import-from #:sxfiler/config)
  (:export #:parse-config-form))
(in-package #:sxfiler/parse-config)

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
