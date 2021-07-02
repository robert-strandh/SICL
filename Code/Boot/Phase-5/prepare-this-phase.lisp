(cl:in-package #:sicl-boot-phase-5)

(defun define-ast-eval (e5)
  (setf (env:fdefinition (env:client e5) e5 'sicl-boot:ast-eval)
        (lambda (ast)
          (let* ((client (env:client e5))
                 (code-object (sicl-compiler:compile-ast client ast)))
            (sicl-compiler:tie-code-object code-object e5)))))

(defun finalize-classes (e3 e4)
  (format *trace-output* "Finalizing all classes in ~a..." (sicl-boot:name e4))
  (finish-output *trace-output*)
  (let ((finalized-p (env:fdefinition (env:client e3) e3 'sicl-clos::class-finalized-p))
        (finalize (env:fdefinition (env:client e3) e3 'sicl-clos:finalize-inheritance)))
    (env:map-defined-classes
     (env:client e4) e4
     (lambda (name class)
       (declare (ignore name))
       (unless (funcall finalized-p class)
         (funcall finalize class)))))
  (format *trace-output* "done~%")
  (finish-output *trace-output*))

(defun prepare-this-phase (e3 e4 e5)
  (define-ast-eval e5)
  (sicl-boot:copy-macro-functions e4 e5)
  (load-source-file "CLOS/class-of-defun.lisp" e4)
  (enable-typep e3 e4)
  (load-source-file "Types/type-of-defgeneric.lisp" e4)
  (enable-object-creation e3 e4)
  (import-functions-from-host-into-e5
   '(cleavir-code-utilities:parse-generic-function-lambda-list
     cleavir-code-utilities:parse-specialized-lambda-list
     cleavir-code-utilities:separate-function-body
     cleavir-code-utilities:required
     intern
     cleavir-code-utilities:parse-deftype
     sicl-method-combination::lambda-list-variables
     sicl-loop::list-car sicl-loop::list-cdr
     env:type-expander
     ;; For Ctype.
     map substitute functionp lognot logcount subsetp member-if intersection
     ;; For Eclector.
     digit-char-p char-upcase
     ;; Eclector uses EVAL in some compiler macros to evaluate some
     ;; Boolean arguments, but it is applied only to contstants so we
     ;; can use the host EVAL.
     eval)
   e3 e5)
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
