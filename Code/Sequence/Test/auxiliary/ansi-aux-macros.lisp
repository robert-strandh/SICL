;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jul  2 07:05:41 2003
;;;; Contains: Macros used in ansi-aux and elsewhere.

(in-package #:sicl-sequence-test)

(declaim (optimize (safety 3)))

;;; Macros to avoid annoying sbcl warning notes

(defmacro handler-case (form &rest cases)
  `(let () (cl:handler-case ,form ,@cases)))

(defmacro handler-bind (handlers &rest body)
  `(let () (cl:handler-bind ,handlers (normally (progn ,@body)))))

;;; Macros for avoiding dead code warnings

(defvar *should-always-be-true* t)

(declaim (notinline should-never-be-called))

(defun should-never-be-called () nil)

(defmacro normally (form &optional (default-form
                                     '(should-never-be-called)))
  `(if *should-always-be-true* ,form ,default-form))

;;; Macro to ignore errors, but report them anyway

(defparameter *report-and-ignore-errors-break* nil
  "When true, REPORT-AND-IGNORE-ERRORS breaks instead of discarding the error condition.")

(defmacro report-and-ignore-errors (&body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (#+sbcl let #+sbcl () #-sbcl progn
       (handler-case
        (progn ,@body)
        (error (condition)
               (princ condition)
               (terpri)
               (when *report-and-ignore-errors-break* (break))
               (values nil condition))))))


