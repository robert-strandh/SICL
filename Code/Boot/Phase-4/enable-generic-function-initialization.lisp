(cl:in-package #:sicl-boot-phase-4)

(defun enable-generic-function-initialization (e2 e3)
  (load-source-file "CLOS/invalidate-discriminating-function.lisp" e3)
  (with-intercepted-function-cells
      (e3
        (make-instance (list #'make-instance))
        (find-class (list (lambda (x) (env:find-class (env:client e2) e2 x)))))
    (load-source-file "CLOS/generic-function-initialization-support.lisp" e3)
    (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e3)))
