(cl:in-package #:sicl-clos)

(defun find-class-named-t ()
  (find-bridge-class t))

(define-symbol-macro *t* (find-class-named-t))
