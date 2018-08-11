(cl:in-package #:sicl-extrinsic-environment)

(defun cst-load-file (filename environment system)
  (format *trace-output* "CST Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (cst-load-source-with-environments
   (asdf:system-relative-pathname :sicl filename)
   environment
   environment
   system))
