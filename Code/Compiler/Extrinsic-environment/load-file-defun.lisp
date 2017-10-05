(cl:in-package #:sicl-extrinsic-environment)

(defun load-file (filename environment)
  (format *trace-output* "Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (load-source-with-environments
   (asdf:system-relative-pathname :sicl-extrinsic-environment filename)
   environment
   environment))

;;; This function is just like LOAD-FILE, except that it calls
;;; CST-LOAD-SOURCE-WITH-ENVIRONMENT rather than
;;; LOAD-SOURCE-WITH-ENVIRONMENTS.
(defun cst-load-file (filename environment)
  (format *trace-output* "CST Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (cst-load-source-with-environments
   (asdf:system-relative-pathname :sicl-extrinsic-environment filename)
   environment
   environment))
