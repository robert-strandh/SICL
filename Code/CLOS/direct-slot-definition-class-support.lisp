(cl:in-package #:sicl-clos)

(defun direct-slot-definition-class-default (class &rest initargs)
  (declare (ignore class initargs))
  (sicl-genv:find-class
   'standard-direct-slot-definition
   (load-time-value (sicl-genv:global-environment))))
