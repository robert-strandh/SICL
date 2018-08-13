(cl:in-package #:sicl-boot)

(defun ld (filename environment1 environment2)
  (format *trace-output* "CST Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (sicl-extrinsic-environment:cst-load-source-with-environments
   (asdf:system-relative-pathname :sicl filename)
   environment1
   environment2
   nil))
