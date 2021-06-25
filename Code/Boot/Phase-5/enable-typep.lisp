(cl:in-package #:sicl-boot-phase-4)

(defun enable-typep (e3 e4)
  (define-error-functions '(typep symbol-package find-package) e4)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:class-precedence-list
        (env:function-cell (env:client e3) e3 'sicl-clos:class-precedence-list)))
    (load-source-file "Types/Typep/typep-atomic.lisp" e4))
  (load-source-file "Types/Typep/typep-compound-integer.lisp" e4)
  (load-source-file "Types/Typep/typep-compound.lisp" e4)
  (load-source-file "Types/Typep/typep.lisp" e4))
