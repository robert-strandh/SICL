(cl:in-package #:sicl-boot-phase-2)

(defun enable-method-combinations (e2 e3 e4)
  (import-functions-from-host
   '(sicl-method-combination:define-method-combination-expander
     sicl-loop::list-car sicl-loop::list-cdr)
   e3)
  (load-source-file "Method-combination/accessor-defgenerics.lisp" e3)
  (load-source-file "Method-combination/method-combination-template-defclass.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (make-instance
           (list (lambda (name-or-class &rest arguments)
                   (if (eq name-or-class 'method-combination)
                       (apply #'make-instance
                              (env:find-class (env:client e2) e2 name-or-class)
                              arguments)
                       (apply #'make-instance name-or-class arguments))))))
    (load-source-file "Method-combination/find-method-combination.lisp" e3))
  (load-source-file "Method-combination/define-method-combination-defmacro.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (make-instance
           (list (lambda (name-or-class &rest arguments)
                   (if (eq name-or-class 'sicl-method-combination:method-combination-template)
                       (apply #'make-instance
                              (env:find-class (env:client e2) e2 name-or-class)
                              arguments)
                       (apply #'make-instance name-or-class arguments)))))
       (find-class (list (lambda (x) (env:find-class (env:client e2) e2 x)))))
    (load-source-file "CLOS/standard-method-combination.lisp" e3)
    (setf (env:fdefinition (env:client e4) e4 'sicl-clos:find-method-combination)
          (lambda (generic-function name arguments)
            (declare (ignore generic-function))
            (funcall (env:fdefinition
                      (env:client e3) e3 'sicl-method-combination:find-method-combination)
                     name arguments)))))
