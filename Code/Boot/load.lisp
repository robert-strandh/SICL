(cl:in-package #:sicl-boot)

(defun ld (filename environment1 environment2)
  (format *trace-output* "Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (sicl-extrinsic-environment:load-source-with-environments
   (asdf:system-relative-pathname :sicl-boot filename)
   environment1
   environment2))
