(cl:in-package #:sicl-clos)

(defun find-class-named-t ()
  (cdr (assoc t *bridge-classes*)))

(define-symbol-macro *t* (find-class-named-t))
