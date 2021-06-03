(cl:in-package #:sicl-boot-phase-3)

(defun define-classp (e3)
  (load-source-file "CLOS/classp-defgeneric.lisp" e3)
  (load-source-file "CLOS/classp-defmethods.lisp" e3))

(defun define-sub-specializer-p (e3)
  (load-source-file "CLOS/sub-specializer-p.lisp" e3))

(defun define-compute-applicable-methods (e3 e4)
  (with-intercepted-function-cells
      (e3
       (class-of
        (env:function-cell (env:client e4) e4 'class-of)))
    (load-source-file "CLOS/compute-applicable-methods-support.lisp" e3))
  (load-source-file "CLOS/compute-applicable-methods-defgenerics.lisp" e3)
  (load-source-file "CLOS/compute-applicable-methods-defmethods.lisp" e3))

(defun define-compute-effective-method (e3)
  (load-source-file "CLOS/compute-effective-method-support.lisp" e3)
  (load-source-file "CLOS/compute-effective-method-defgenerics.lisp" e3)
  (load-source-file "CLOS/compute-effective-method-defmethods.lisp" e3))

(defun define-compute-discriminating-function (e2 e3 e4)
  (load-source-file "CLOS/compute-discriminating-function-defgenerics.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (class-of (env:function-cell (env:client e2) e2 'class-of))
       (make-instance (env:function-cell (env:client e2) e2 'make-instance)))
    (load-source-file "CLOS/maybe-replace-method.lisp" e3))
  (with-intercepted-function-cells
      (e3
       (class-of (env:function-cell (env:client e4) e4 'class-of)))
    (load-source-file "CLOS/compute-discriminating-function-support.lisp" e3))
  (load-source-file "CLOS/discriminating-automaton.lisp" e3)
  (define-error-functions '(sicl-clos::compute-test-tree) e3)
  (load-source-file "CLOS/discriminating-tagbody.lisp" e3)
  (define-error-functions '(sicl-clos::make-cdr) e3)
  (load-source-file "CLOS/compute-discriminating-function-support-c.lisp" e3)
  (load-source-file "CLOS/compute-discriminating-function-defmethods.lisp" e3))

(defun enable-compute-discriminating-function (e2 e3 e4)
  (define-classp e3)
  (define-sub-specializer-p e3)
  (define-compute-applicable-methods e3 e4)
  (define-compute-effective-method e3)
  (import-functions-from-host
   '(no-applicable-method)
   e3)
  (setf (env:fdefinition (env:client e3) e3 'compile)
        (lambda (x lambda-expression)
          (assert (null x))
          (assert (and (consp lambda-expression) (eq (first lambda-expression) 'lambda)))
          (with-intercepted-function-cells
              (e3
               (make-instance
                   (env:function-cell (env:client e2) e2 'make-instance)))
            (let* ((cst (cst:cst-from-expression lambda-expression))
                   (ast (cleavir-cst-to-ast:cst-to-ast (env:client e3) cst e3)))
              (funcall (env:fdefinition (env:client e3) e3 'sicl-boot:ast-eval)
                       ast)))))
  (setf (env:fdefinition
         (env:client e3) e3 'sicl-clos:set-funcallable-instance-function)
        #'sicl-host-mop:set-funcallable-instance-function)
  (define-compute-discriminating-function e2 e3 e4))

(defun enable-defgeneric (e2 e3 e4)
  (setf (env:fdefinition (env:client e2) e2 'sicl-clos:class-prototype)
        #'sicl-host-mop:class-prototype)
  (with-intercepted-function-cells
      (e3
       (class-of (env:function-cell (env:client e2) e2 'class-of))
       (make-instance (env:function-cell (env:client e2) e2 'make-instance))
       (sicl-clos:class-precedence-list
        (env:function-cell (env:client e2) e2 'sicl-clos:class-precedence-list))
       (sicl-clos:class-prototype
        (env:function-cell (env:client e2) e2 'sicl-clos:class-prototype)))
    (load-source-file "CLOS/ensure-generic-function-using-class-support.lisp" e3))
  (load-source-file "CLOS/ensure-generic-function-using-class-defgenerics.lisp" e3)
  (with-intercepted-function-cells
      (e3
       ((setf fdefinition)
        (env:function-cell (env:client e4) e4 '(setf fdefinition))))
    (load-source-file "CLOS/ensure-generic-function-using-class-defmethods.lisp" e3))
  (with-intercepted-function-cells
      (e4
       (sicl-clos:ensure-generic-function-using-class
        (env:function-cell
         (env:client e3) e3 'sicl-clos:ensure-generic-function-using-class)))
    (load-source-file "CLOS/ensure-generic-function-defun.lisp" e4))
  (load-source-file "CLOS/defgeneric-defmacro.lisp" e4))

(defun define-generic-function-class-names (e4)
  (setf (env:fdefinition
         (env:client e4) e4 'sicl-clos::generic-function-class-names)
        (lambda (name environment)
          (declare (ignore name environment))
          (values 'standard-generic-function 'standard-method))))
