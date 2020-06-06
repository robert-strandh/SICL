(cl:in-package #:sicl-boot-phase-8)

(defmethod print-object ((object sicl-boot::header) stream)
  (handler-case
      (funcall (sicl-genv:fdefinition 'print-object sicl-boot::*e5*)
               object
               stream)
    (error () (format stream "[unprintable]"))))
