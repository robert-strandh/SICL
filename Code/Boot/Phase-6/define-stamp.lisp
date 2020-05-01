(cl:in-package #:sicl-boot-phase-6)

(defun define-stamp (e6)
  (setf (sicl-genv:fdefinition 'sicl-clos::stamp e6)
        (sicl-genv:fdefinition 'sicl-clos::stamp sicl-boot:*e3*)))
