(cl:in-package #:sicl-boot-phase-6)

(defun enable-typep (e5)
  (let ((client (env:client e5)))
    (setf (env:fdefinition client e5 'sicl-type:type-expander)
          (lambda (type-descriptor)
            (env:type-expander client e5 type-descriptor))))
  (load-source-file "Types/Typep/typep-atomic.lisp" e5)
  (load-source-file "Types/Typep/typep-compound-integer.lisp" e5)
  (load-source-file "Types/Typep/typep-compound.lisp" e5)
  (load-source-file "Types/Typep/typep.lisp" e5))
