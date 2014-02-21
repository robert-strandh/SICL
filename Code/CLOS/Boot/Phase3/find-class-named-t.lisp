(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'find-class-named-t))

(defun find-class-named-t ()
  (find-target-class t))
