(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'class-of))

(defun class-of (object)
  (if (heap-instance-p object)
      (heap-instance-class object)
      (cdr (assoc t *classes*))))
