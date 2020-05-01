(cl:in-package #:sicl-boot-phase-5)

(defun define-stamp (e5)
  (setf (sicl-genv:fdefinition 'sicl-clos::stamp e5)
        (sicl-genv:fdefinition 'sicl-clos::stamp sicl-boot:*e3*)))
