(cl:in-package #:sicl-compiler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function COMPILE-FILE.

(defun compile-file
    (input-file
     &key
       (output-file nil output-file-p)
       (verbose *compile-verbose*)
       (print *compile-print*)
       (external-format :default))
  (declare (ignore output-file output-file-p verbose print))
  (with-open-file
      (stream input-file
              :direction :input
              :external-format external-format)
    (let* ((*compile-file-pathname* (merge-pathnames input-file))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (*package* *package*)
           ;; FIXME: should this bind the *READTABLE* in Eclector?
           (*readtable* *readtable*)
           (*cross-compiling-p* nil)
           (*top-level-asts* '()))
      (loop with eof-value = (list nil)
            for form = (eclector.reader:read stream nil eof-value)
            until (eq form eof-value)
            do (process-top-level-form form nil)))))
