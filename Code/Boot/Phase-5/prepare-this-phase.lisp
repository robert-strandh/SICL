(cl:in-package #:sicl-boot-phase-5)

(defun finalize-classes (e3 e4)
  (format *trace-output* "Finalizing all classes in ~a..." (sicl-boot:name e4))
  (finish-output *trace-output*)
  (let ((visited (make-hash-table :test #'eq))
        (finalized-p (env:fdefinition (env:client e3) e3 'sicl-clos::class-finalized-p))
        (finalize (env:fdefinition (env:client e3) e3 'sicl-clos:finalize-inheritance)))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (let ((class (env:find-class (env:client e4) e4 symbol)))
          (unless (or (null class) (funcall finalized-p class))
            (funcall finalize class))))))
  (format *trace-output* "done~%")
  (finish-output *trace-output*))

(defun prepare-this-phase (e3 e4 e5)
  (load-source-file "Types/type-of-defgeneric.lisp" e4)
  (enable-object-creation e3 e4)
  (enable-method-combinations e3 e4 e5)
  (setf (env:special-operator (env:client e5) e5 'cleavir-primop:multiple-value-call) t)
  (setf (env:fdefinition (env:client e4) e4 'compile)
        (lambda (x lambda-expression)
          (assert (null x))
          (assert (and (consp lambda-expression) (eq (first lambda-expression) 'lambda)))
          (with-intercepted-function-cells
              (e4
               (make-instance
                   (env:function-cell (env:client e3) e3 'make-instance)))
            (let* ((cst (cst:cst-from-expression lambda-expression))
                   (ast (cleavir-cst-to-ast:cst-to-ast (env:client e4) cst e4)))
              (funcall (env:fdefinition (env:client e4) e4 'sicl-boot:ast-eval)
                       ast)))))
  (enable-compute-discriminating-function e3 e4 e5)
  (load-source-file "CLOS/defgeneric-support.lisp" e5)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:set-funcallable-instance-function
        (list #'sicl-host-mop:set-funcallable-instance-function)))
    (load-source-file "CLOS/invalidate-discriminating-function.lisp" e4))
  (import-functions-from-host
   '(cleavir-code-utilities:parse-generic-function-lambda-list
     cleavir-code-utilities:required)
   e4)
  (load-source-file "CLOS/generic-function-initialization-support.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:method-function
        (env:function-cell (env:client e3) e3 'sicl-clos:method-function)))
    (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e4))
  (enable-defgeneric e3 e4 e5)
  (enable-defmethod e3 e4 e5)
  (enable-defclass e3 e4 e5)
  (finalize-classes e3 e4))
