(cl:in-package #:sicl-clos)

(defun reader-method-class-default (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  (sicl-genv:find-class
   'standard-reader-method
   (load-time-value (sicl-genv:global-environment))))

(defun writer-method-class-default (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  (sicl-genv:find-class
   'standard-writer-method
   (load-time-value (sicl-genv:global-environment))))

