(cl:in-package #:sicl-boot-phase-6)

(defun enable-typep (e3 e5)
  (import-functions-from-host-into-e5 '(env:type-expander) e3 e5)
  (load-source-file "Types/type-expander-defun.lisp" e5)  
  (define-error-functions '(typep sicl-type::typep-atomic) e5)
  (load-source-file "Types/Typep/typep-atomic.lisp" e5)
  (load-source-file "Types/Typep/typep-compound-integer.lisp" e5)
  (load-source-file "Types/Typep/typep-compound.lisp" e5)
  (load-source-file "Types/Typep/typep.lisp" e5))

