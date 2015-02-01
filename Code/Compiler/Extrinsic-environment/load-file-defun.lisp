(cl:in-package #:sicl-extrinsic-environment)

(defun load-file (filename environment)
  (load-source-with-environments
   (asdf:system-relative-pathname :sicl-extrinsic-hir-compiler filename)
   environment
   environment))
