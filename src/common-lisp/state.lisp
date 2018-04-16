;;; Define struct for single-source-of-truth state.
(in-package :cl-user)
(defpackage #:sxfiler/state
  (:use #:cl)
  (:export #:make-state))
(in-package #:sxfiler/state)

(defstruct root-state
  "The state that is root of the all state of sxfiler.
The struct of root contains many status below.

- current active pane
- pane information
  - all pane information

"
  (active-pane :left :type symbol)
  (left-pane (make-)))
