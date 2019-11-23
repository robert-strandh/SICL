(cl:in-package #:sicl-clos)

;;; FIXME: this code is almost certainly wrong.
(defun validate-superclass-default (class superclass)
  (or (eq superclass *t*)
      (let ((c1 (class-of class))
            (c2 (class-of superclass)))
        (or (eq c1 c2)
            (and (eq c1 *standard-class*)
                 (eq c2 *funcallable-standard-class*))
            (and (eq c1 *funcallable-standard-class*)
                 (eq c2 *standard-class*))))))
