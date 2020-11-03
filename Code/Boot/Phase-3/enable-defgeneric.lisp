(cl:in-package #:sicl-boot-phase-3)

(defun define-classp (e3)
  (load-source-file "CLOS/classp-defgeneric.lisp" e3)
  (load-source-file "CLOS/classp-defmethods.lisp" e3))

(defun define-sub-specializer-p (e3)
  (load-source-file "CLOS/sub-specializer-p.lisp" e3))

(defun define-compute-applicable-methods (e3)
  (with-intercepted-function-cells
      (e3
       (class-of
        (list (lambda (object)
                (class-of object)))))
    (load-source-file "CLOS/compute-applicable-methods-support.lisp" e3))
  (load-source-file "CLOS/compute-applicable-methods-defgenerics.lisp" e3)
  (load-source-file "CLOS/compute-applicable-methods-defmethods.lisp" e3))

(defun define-compute-effective-method (e3)
  (load-source-file "CLOS/compute-effective-method-support.lisp" e3)
  (load-source-file "CLOS/compute-effective-method-defgenerics.lisp" e3)
  (load-source-file "CLOS/compute-effective-method-defmethods.lisp" e3))

(defun define-compute-discriminating-function (e3)
  (load-source-file "CLOS/compute-discriminating-function-defgenerics.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (class-of
        (list (lambda (object)
                (cond ((typep object 'sicl-boot::header)
                       (slot-value object 'sicl-boot::%class))
                      ((null object)
                       (env:find-class (env:client e3) e3 'null))
                      ((symbolp object)
                       (env:find-class (env:client e3) e3 'symbol))
                      ((consp object)
                       (env:find-class (env:client e3) e3 'cons))
                      ((functionp object)
                       (env:find-class (env:client e3) e3 'function))
                      (t
                       (class-of object)))))))
    (load-source-file "CLOS/compute-discriminating-function-support.lisp" e3))
  (load-source-file "CLOS/discriminating-automaton.lisp" e3)
  (define-error-functions '(sicl-clos::compute-test-tree) e3)
  (load-source-file "CLOS/discriminating-tagbody.lisp" e3)
  (define-error-functions '(sicl-clos::make-cdr) e3)
  (load-source-file "CLOS/compute-discriminating-function-support-c.lisp" e3)
  (load-source-file "CLOS/compute-discriminating-function-defmethods.lisp" e3))

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

(defun enable-compute-discriminating-function (e3)
  (define-classp e3)
  (define-sub-specializer-p e3)
  (define-compute-applicable-methods e3)
  (define-compute-effective-method e3)
  (import-functions-from-host
   '(no-applicable-method)
   e3)
  (setf (env:fdefinition (env:client e3) e3 'compile)
        (lambda (x lambda-expression)
          (assert (null x))
          (assert (and (consp lambda-expression) (eq (first lambda-expression) 'lambda)))
          (let* ((cst (cst:cst-from-expression lambda-expression))
                 (ast (cleavir-cst-to-ast:cst-to-ast (env:client e3) cst e3)))
            (ast-eval ast (env:client e3) e3))))
  (setf (env:fdefinition
         (env:client e3) e3 'sicl-clos:set-funcallable-instance-function)
        #'closer-mop:set-funcallable-instance-function)
  (define-compute-discriminating-function e3))

(defun enable-defgeneric (e2 e4)
  (let ((client (env:client e4)))
    (setf (env:fdefinition client e4 'ensure-generic-function)
          (lambda (function-name &rest arguments &key &allow-other-keys)
            (let ((args (copy-list arguments)))
              (loop while (remf args :environment))
              (loop while (remf args :generic-function-class))
              (loop while (remf args :method-class))
              (if (env:fboundp client e4 function-name)
                  (env:fdefinition client e4 function-name)
                  (setf (env:fdefinition client e4 function-name)
                        (apply #'make-instance
                               (env:find-class client e2 'standard-generic-function)
                               :name function-name
                               :method-class (env:find-class (env:client e2) e2 'standard-method)
                               args)))))))
  (load-source-file "CLOS/defgeneric-defmacro.lisp" e4))

(defun define-generic-function-class-names (e4)
  (setf (env:fdefinition
         (env:client e4) e4 'sicl-clos::generic-function-class-names)
        (lambda (name environment)
          (declare (ignore name environment))
          (values 'standard-generic-function 'standard-method))))
