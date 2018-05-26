;;; Define operations take snapshot and restore it for STATE.
(in-package #:cl-user)
(defpackage #:sxfiler/snapshot
  (:use #:cl)
  (:import-from #:sxfiler/types/pane
                #:pane-location)
  (:import-from #:sxfiler/types/pane-history
                #:pane-history-records
                #:pane-history-location)
  (:import-from #:sxfiler/state
                #:state-active-pane
                #:state-inactive-pane
                #:state-active-pane-history
                #:state-inactive-pane-history)
  (:export #:+snapshot-directory+
           #:+snapshot-file-path+
           #:save-snapshot
           #:restore-snapshot))
(in-package #:sxfiler/snapshot)

(defvar +snapshot-directory+ (uiop:merge-pathnames* "sxfiler/" (uiop:xdg-config-home))
  "The directory from to save and restore snapshot.")
(defvar +snapshot-file-path+ (uiop:merge-pathnames* "snapshot.dat" +snapshot-directory+)
  "The file path for snapshot.")

;; Snapshot is subset of state structure each type

(defstruct (snapshot (:constructor %make-snapshot))
  "The snapshot of the STATE structure to be able to restore state without
data to take with side effects.
"
  (active-pane-location :left :type (member :left :right))
  (left-pane-history nil :type sxfiler/types/pane-history:pane-history)
  (right-pane-history nil :type sxfiler/types/pane-history:pane-history))

(defun choice-by-location (wished-location value-of-active value-of-inactive)
  (let ((active-location (pane-history-location value-of-active)))
    (cond ((eql wished-location active-location) value-of-active)
          (t value-of-inactive))))

(defun state->snapshot (state)
  "Convert from the STATE to a snapshot."
  (check-type state sxfiler/state:state)
  (let ((active-pane (state-active-pane state))
        (active-pane-history (state-active-pane-history state))
        (inactive-pane-history (state-inactive-pane-history state)))
    (%make-snapshot :active-pane-location (pane-location active-pane)
                    :left-pane-history (choice-by-location :left
                                                           active-pane-history
                                                           inactive-pane-history)
                    :right-pane-history (choice-by-location :right
                                                            active-pane-history
                                                            inactive-pane-history))))

(defun restore-pane-from-latest-history (history)
  (let ((latest-history (first (pane-history-records history)))
        (location (pane-history-location history)))
    (if (not (null latest-history))
        (let ((pane (sxfiler/types/pane:make-pane :location location
                                                  :directory (sxfiler/types/pane-history:record-directory latest-history))))
          (sxfiler/types/pane:renew-file-list pane))
        (sxfiler/types/pane:make-pane :location location))))

(defun snapshot->state (snapshot)
  "Convert SNAPSHOT to the state."
  (check-type snapshot snapshot)
  (let ((left-pane (restore-pane-from-latest-history (snapshot-left-pane-history snapshot)))
        (right-pane (restore-pane-from-latest-history (snapshot-right-pane-history snapshot)))
        (location (snapshot-active-pane-location snapshot)))
    (sxfiler/state:make-state :active-pane (ecase location
                                             (:left left-pane)
                                             (:right right-pane))
                              :inactive-pane (ecase location
                                               (:left left-pane)
                                               (:right right-pane))
                              :active-pane-history (ecase location
                                                     (:left (snapshot-left-pane-history snapshot))
                                                     (:right (snapshot-right-pane-history snapshot)))
                              :inactive-pane-history (ecase location
                                                       (:left (snapshot-left-pane-history snapshot))
                                                       (:right (snapshot-right-pane-history snapshot))))))

(defun save-snapshot (stream state)
  "Save marshalled snapshot created by STATE to STREAM."
  (check-type stream stream)
  (check-type state sxfiler/state:state)
  (format stream "~S" (marshal:marshal (state->snapshot state))))

(defun restore-snapshot (stream)
  "Restore the state from unmashalling snapshot is marshalled that read from STREAM."
  (check-type stream stream)
  (snapshot->state (marshal:unmarshal (uiop:slurp-stream-form stream))))
