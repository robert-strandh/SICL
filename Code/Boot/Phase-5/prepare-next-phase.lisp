(cl:in-package #:sicl-boot-phase-5)

(defun prepare-next-phase (e3 e4 e5)
  (load-source-file "CLOS/class-of-defun.lisp" e5)
  (enable-typep e3 e5)
  (load-source-file "Types/type-of-defgeneric.lisp" e5)
  (enable-object-creation e3 e5)
  (setf (env:fdefinition (env:client e5) e5 'compile)
        (lambda (x lambda-expression)
          (assert (null x))
          (assert (and (consp lambda-expression) (eq (first lambda-expression) 'lambda)))
          (let* ((cst (cst:cst-from-expression lambda-expression))
                 (ast (cleavir-cst-to-ast:cst-to-ast (env:client e5) cst e5)))
            (with-intercepted-function-cells
                (e5
                 (make-instance
                  (env:function-cell (env:client e3) e3 'make-instance))
                 (sicl-clos:method-function
                  (env:function-cell (env:client e4) e4 'sicl-clos:method-function)))
              (funcall (env:fdefinition (env:client e5) e5 'sicl-boot:ast-eval)
                       ast))))))
