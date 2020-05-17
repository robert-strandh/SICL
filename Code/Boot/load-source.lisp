(cl:in-package #:sicl-boot)

(defun load-source (relative-pathname environment)
  (compile-file (sicl-genv:client *e0*) relative-pathname *e0*)
  (load-fasl relative-pathname environment))

                
