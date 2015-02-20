(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun load-file (filename environment)
  (format *trace-output* "Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (sicl-extrinsic-environment:load-source-with-environments
   (asdf:system-relative-pathname :sicl-extrinsic-hir-compiler filename)
   environment
   environment))
