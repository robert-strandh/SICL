(cl:in-package #:sicl-boot-phase-3)

(defun enable-typep (e2 e3)
  ;; FIXME: define functions SYMBOL-PACKAGE and FIND-PACKAGE in the
  ;; environment so that they exist at this point.
  (define-error-functions '(typep symbol-package find-package) e3)
  (with-intercepted-function-cells
      (e3
       (sicl-clos:class-precedence-list
        (env:function-cell (env:client e2) e2 'sicl-clos:class-precedence-list)))
    (load-source-file "Types/Typep/typep-atomic.lisp" e3))
  (load-source-file "Types/Typep/typep-compound-integer.lisp" e3)
  (load-source-file "Types/Typep/typep-compound.lisp" e3)
  (load-source-file "Types/Typep/typep.lisp" e3))
