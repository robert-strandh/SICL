(cl:in-package #:sicl-boot-phase-3)

(defun finalize-classes (e3)
  (loop for name in '(standard-generic-function symbol)
        for class = (env:find-class (env:client e3) e3 name)
        do (closer-mop:finalize-inheritance class)))

(defun prepare-next-phase (e2 e3 e4)
  (setf (env:fdefinition (env:client e4) e4 'sicl-boot:ast-eval)
        (lambda (ast)
          (let ((code (sicl-ast-evaluator:translate-top-level-ast (env:client e4) ast)))
            (funcall (compile nil code) e4))))
  (sicl-boot:copy-macro-functions e3 e4)
  (load-source-file "CLOS/class-of-defun.lisp" e3)
  (enable-typep e2 e3)
  (enable-object-creation e2 e3)
  (enable-method-combinations e2 e3 e4)
  (setf (env:special-operator (env:client e4) e4 'cleavir-primop:multiple-value-call) t)
  (enable-compute-discriminating-function e3 e4)
  (load-source-file "CLOS/defgeneric-support.lisp" e4)
  (load-source-file "CLOS/invalidate-discriminating-function.lisp" e3)
  ;; (setf (env:fdefinition (env:client e3) e3 'slot-value)
  ;;       (lambda (object slot-name)
  ;;         (error "SLOT-VALUE called with ~s and ~s" object slot-name)))
  ;; (setf (env:fdefinition (env:client e3) e3 '(setf slot-value))
  ;;       (lambda (value object slot-name)
  ;;         (error "(SETF SLOT-VALUE) called with ~s, ~s and ~s" value object slot-name)))
  (import-functions-from-host
   '(shared-initialize initialize-instance)
   e3)
  (import-functions-from-host
   '(cleavir-code-utilities:parse-generic-function-lambda-list
     cleavir-code-utilities:required)
   e3)
  (load-source-file "CLOS/generic-function-initialization-support.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (sicl-clos:method-function
        (env:function-cell (env:client e2) e2 'sicl-clos:method-function)))
    (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e3))
  (enable-defgeneric e2 e3 e4)
  (enable-defmethod e2 e3 e4)
  (enable-defclass e2 e3 e4)
  (finalize-classes e3))
