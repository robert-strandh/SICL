(cl:in-package #:sicl-boot-phase-7)

(defun load-source (relative-pathname environment)
  (sicl-boot:compile-file
   relative-pathname
   (sicl-genv:client sicl-boot:*e0*)
   environment)
  (load-fasl relative-pathname environment))
