(cl:in-package #:sicl-boot-phase-4)

(defun define-stamp (e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::stamp e4)
        (sicl-genv:fdefinition 'sicl-clos::stamp sicl-boot:*e3*)))
