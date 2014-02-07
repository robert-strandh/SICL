(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'class-of))

(defun class-of (object)
  (heap-instance-class object))
