;;;; completer package for candidates completion
(in-package #:cl-user)
(defpackage #:sxfiler/completer
  (:use #:cl)
  (:export #:completer
           #:make-completer
           #:matcher
           #:get-matcher

           #:complete))
(in-package #:sxfiler/completer)

(defstruct matcher)
(defstruct (forward-exact-matcher (:include matcher)))
(defstruct (partial-matcher (:include matcher)))

(defmethod match-from-candidates (matcher candidates input)
  "Return items apply matcher. Default implementation returns argument directly"
  (declare (ignorable input matcher))
  candidates)

(defmethod match-from-candidates ((matcher forward-exact-matcher) candidates input)
  "Return items that are matched candidates with input forward exactly.
If no any matched item, return nil
"
  (let ((regex (concatenate 'string "^" (cl-ppcre:quote-meta-chars input))))
    (remove-if (lambda (v)
                 (null (cl-ppcre:scan regex v)))
               candidates)))

(defmethod match-from-candidates ((matcher partial-matcher) candidates input)
  "return items that are matched candidates with input partially"
  (let ((regex (cl-ppcre:quote-meta-chars input)))
    (remove-if (lambda (v)
                 (null (cl-ppcre:scan regex v)))
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
        (t (error (make-condition 'unknown-matcher :kind kind)))))

;; Completer structure. This has matcher type, candidates, and cursor position
;; current selected.
(defstruct completer
  (matcher (make-forward-exact-matcher) :type matcher)
  (candidates '() :type list)
  (matched-items (make-array 0) :type vector)
  (cursor 0 :type (unsigned-byte 32)))

(defun complete (completer input)
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
