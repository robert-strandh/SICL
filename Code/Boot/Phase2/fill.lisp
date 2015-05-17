(cl:in-package #:sicl-boot-phase2)

(defun ld (filename environment)
  (format *trace-output* "Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (sicl-extrinsic-environment:load-source-with-environments
   (asdf:system-relative-pathname :sicl-boot-phase2 filename)
   (sicl-boot-phase1:compilation-environment (phase1-environment environment))
   environment))

(defun fill-environment (environment)
  (declare (ignore environment)))
