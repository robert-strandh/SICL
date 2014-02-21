(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'make-initfunction))

(defun make-initfunction (form)
  `(load-time-value (compile nil `(lambda () ,',form))))
