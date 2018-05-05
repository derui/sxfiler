;;;; completer package for candidates completion
(in-package #:cl-user)
(defpackage #:sxfiler/completer
  (:use #:cl)
  (:export #:completer
           #:make-completer
           #:matcher
           #:get-matcher
           #:update-candidates

           #:complete
           #:select-prev-matched
           #:select-next-matched))
(in-package #:sxfiler/completer)

(defstruct matcher)
(defstruct (forward-exact-matcher (:include matcher)))
(defstruct (partial-matcher (:include matcher)))

(defmethod match-from-candidates (matcher candidates input)
  "Return items apply matcher. Default implementation returns argument directly"
  (declare (ignorable input matcher))
  candidates)

(defun scan-item-with-regex (regex item)
  "Scan item with regex. Patterns of item allow to use this function below.
 - string
 - string-first-list
"
  (cond ((stringp item)
         (null (cl-ppcre:scan regex item)))
        ((and (consp item)
              (stringp (first item)))
         (null (cl-ppcre:scan regex (first item))))
        (t t)))

(defmethod match-from-candidates ((matcher forward-exact-matcher) candidates input)
  "Return items that are matched candidates with input forward exactly.
If no any matched item, return nil
"
  (let ((regex (concatenate 'string "^" (cl-ppcre:quote-meta-chars input))))
    (remove-if (lambda (v) (scan-item-with-regex regex v))
               candidates)))

(defmethod match-from-candidates ((matcher partial-matcher) candidates input)
  "return items that are matched candidates with input partially"
  (let ((regex (cl-ppcre:quote-meta-chars input)))
    (remove-if (lambda (v) (scan-item-with-regex regex v))
               candidates)))

(define-condition unknown-matcher-error (error)
  ((kind :initarg :kind
         :reader unknown-matcher-error-kind))
  (:report (lambda (condition stream)
             (format stream "Unknown matcher kind: ~A" (unknown-matcher-error-kind condition)))))

(defun get-matcher (kind)
  "Get matcher specified KIND. Allow values for KIND are below.

:FORWARD-EXACT -> Return matcher for forward-exact matching
:PARTIAL -> Return matcher for partial matching

If KIND has other symbol defined above, raise error with `unknown-matcher-error' condition
"
  (check-type kind symbol)
  (cond ((eql kind :forward-exact) (make-forward-exact-matcher))
        ((eql kind :partial) (make-partial-matcher))
        (t (error (make-condition 'unknown-matcher-error :kind kind)))))

;; Completer structure. This has matcher type, candidates, and cursor position
;; current selected.
;; Candidates in completer is list, then it is able to contain one element type below.
;;   - `string'
;;   - string-first-list
(defstruct completer
  (matcher (make-forward-exact-matcher) :type matcher)
  (candidates '() :type list)
  (matched-items (make-array 0) :type vector)
  (cursor 0 :type (unsigned-byte 32)))

;; Encoder of yason for completer will write matched items only
(defmethod yason:encode ((object completer) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:with-object-element ("matchedItems")
        (let ((matched-items (mapcar (lambda (v)
                                       (cond ((stringp v) v)
                                             ((and (consp v)
                                                   (stringp (car v)))
                                              (cdr v))
                                             (t nil)))
                                     (coerce (completer-matched-items object) 'list))))
          (yason:encode-array-element matched-items))))))

(defun update-candidates (completer candidates)
  "Get new completer with given CANDIDATES.
Returning completer has same matcher of COMPLETER.
"
  (check-type completer completer)
  (check-type candidates list)
  (make-completer :matcher (completer-matcher completer)
                  :candidates candidates))

(defun complete (completer input)
  "Search INPUT in candidates of COMPLETER, and return result searching.
If INPUT is empty string, return COMPLETER.
"
  (check-type completer completer)
  (check-type input string)
  (if (string= "" input)
      completer
      (let* ((cloned (copy-completer completer))
             (matcher (completer-matcher completer))
             (candidates (completer-candidates completer))
             (matched-items (match-from-candidates matcher candidates input)))
        (setf (completer-matched-items cloned) (coerce matched-items 'vector))
        (setf (completer-cursor cloned) 0)
        cloned)))

;; Cursor management
(defun update-cursor-pos (completer next-cursor-function)
  "Update cursor position and return updated completer. This function is immutable."
  (let* ((size-of-matched (length (completer-matched-items completer)))
         (current-cursor (completer-cursor completer))
         (updated-cursor-pos (funcall next-cursor-function size-of-matched current-cursor))
         (cloned (copy-structure completer)))
    (setf (completer-cursor cloned) updated-cursor-pos)
    cloned))

(defun select-next-matched (completer)
  "Move cursor of COMPLETER to next matched."
  (check-type completer completer)
  (update-cursor-pos completer (lambda (len cursor) (min (max 0 (1- len)) (1+ cursor)))))

(defun select-prev-matched (completer)
  "Move cursor of COMPLETER to next matched."
  (check-type completer completer)
  (update-cursor-pos completer (lambda (len cursor)
                                 (declare (ignorable len))
                                 (max 0 (1- cursor)))))

(defun get-selected (completer)
  "Get matched item targetting COMPLETER's cursor.
If matched items are empty, return nil.
"
  (check-type completer completer)
  (when (< 0 (length (completer-matched-items completer)))
    (aref (completer-matched-items completer) (completer-cursor completer))))
