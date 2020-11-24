(cl:in-package #:sicl-boot-phase-2)

(defun enable-typep (e2)
  (define-error-functions '(typep symbol-package find-package) e2)
  (with-intercepted-function-cells
      (e2
       (sicl-clos:class-precedence-list
        (list #'closer-mop:class-precedence-list))
       (class-of
        (list #'class-of)))
    (load-source-file "Types/Typep/typep-atomic.lisp" e2)
    (load-source-file "Types/Typep/typep-compound-integer.lisp" e2)
    (load-source-file "Types/Typep/typep-compound.lisp" e2)
    (load-source-file "Types/Typep/typep.lisp" e2)))
