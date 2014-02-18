(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'set-funcallable-instance-function))

(defun set-funcallable-instance-function (funcallable-instance function)
  ;; FIXME: Copy the slots of the function to the funcallable
  ;; instance.
  (declare (ignore funcallable-instance function))
  nil)
