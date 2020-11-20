(cl:in-package #:sicl-boot-phase-4)

(defun ast-eval (ast client environment)
  (let* ((global-environment (trucler:global-environment client environment))
         (hir (sicl-ast-to-hir:ast-to-hir client ast))
         (fun (sicl-hir-evaluator:top-level-hir-to-host-function client hir))
         (sicl-run-time:*dynamic-environment* '())
         (function-cell-function
           (sicl-environment:fdefinition
            client global-environment 'sicl-data-and-control-flow:function-cell)))
    (funcall fun
             (apply #'vector
                    nil ; Ultimately, replace with code object.
                    #'sicl-hir-evaluator:enclose
                    #'sicl-hir-evaluator:initialize-closure
                    #'cons
                    nil
                    (append (loop with names = (sicl-hir-transformations:function-names hir)
                                  for name in names
                                  collect (funcall function-cell-function name))
                            (sicl-hir-transformations:constants hir))))))

(defun prepare-next-phase (e2 e3 e4 e5)
  (setf (env:fdefinition (env:client e5) e5 'sicl-boot:ast-eval)
        (lambda (ast)
          (ast-eval ast (env:client e5) e5)))
  (sicl-boot:copy-macro-functions e3 e5)
  (enable-object-creation e3 e4)
  (enable-method-combinations e3 e4 e5)
  (setf (env:special-operator (env:client e5) e5 'cleavir-primop:multiple-value-call) t)
  ;; Consider whether this definition, currently global, should be
  ;; defined with an ovverriding function cell instead.
  (setf (env:fdefinition (env:client e4) e4 'compile)
        (lambda (x lambda-expression)
          (assert (null x))
          (assert (and (consp lambda-expression) (eq (first lambda-expression) 'lambda)))
          (let* ((cst (cst:cst-from-expression lambda-expression))
                 (ast (cleavir-cst-to-ast:cst-to-ast (env:client e4) cst e4)))
            (with-intercepted-function-cells
                (e4
                 (make-instance
                     (list (lambda (name-or-class &rest initargs)
                             (let ((class (if (symbolp name-or-class)
                                              (env:find-class (env:client e2) e2 name-or-class)
                                              name-or-class)))
                               (apply #'make-instance class initargs))))))
              (ast-eval ast (env:client e4) e4)))))
  (enable-compute-discriminating-function e3 e4)
  (setf (env:fdefinition (env:client e5) e5 'find-method-combination)
        (lambda (name arguments)
          (funcall (env:fdefinition (env:client e4) e4 'sicl-method-combination:find-method-combination)
                   name arguments)))
  (setf (env:macro-function (env:client e5) e5 'sicl-clos:find-method-combination)
        (lambda (form environment)
          (declare (ignore environment))
          `(find-method-combination ,(third form) ,(fourth form))))
  (load-source-file "CLOS/defgeneric-support.lisp" e5)
  ;; (import-functions-from-host
  ;;  '(cleavir-code-utilities:parse-generic-function-lambda-list
  ;;    cleavir-code-utilities:required)
  ;;  e4)
  ;; (setf (env:fdefinition (env:client e4) e4 'sicl-clos:set-funcallable-instance-function)
  ;;       #'closer-mop:set-funcallable-instance-function)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:set-funcallable-instance-function
        (list #'closer-mop:set-funcallable-instance-function)))
    (load-source-file "CLOS/invalidate-discriminating-function.lisp" e4))
  (import-functions-from-host
   '(cleavir-code-utilities:parse-generic-function-lambda-list
     cleavir-code-utilities:required)
   e4)
  (load-source-file "CLOS/generic-function-initialization-support.lisp" e4)
  (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e4)
  (enable-defgeneric e3 e5)
  (enable-defmethod e3 e4 e5)
  (enable-defclass e3 e4 e5))
