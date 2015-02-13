(cl:in-package #:sicl-extrinsic-environment)

(defun load-file (filename environment)
  (format *trace-output* "Loading file ~a~%" filename)
  (load-source-with-environments
   (asdf:system-relative-pathname :sicl-extrinsic-environment filename)
   environment
   environment))
