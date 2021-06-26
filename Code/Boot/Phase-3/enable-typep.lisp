(cl:in-package #:sicl-boot-phase-2)

(defun enable-typep (e2)
  (define-error-functions '(typep symbol-package find-package) e2)
  (load-source-file "Types/Typep/typep-atomic.lisp" e2)
  (load-source-file "Types/Typep/typep-compound-integer.lisp" e2)
  (load-source-file "Types/Typep/typep-compound.lisp" e2)
  (load-source-file "Types/Typep/typep.lisp" e2))
