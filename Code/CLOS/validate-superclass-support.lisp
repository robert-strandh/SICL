(cl:in-package #:sicl-clos)

(defun validate-superclass-default (class superclass)
  (let ((c1 (class-of class))
        (c2 (class-of superclass))
        (built-in-class (find-class 'built-in-class)))
    (or (eq c1 built-in-class)
        (not (eq c2 built-in-class)))))
