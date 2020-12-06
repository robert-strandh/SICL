(cl:in-package #:sicl-boot-phase-5)

(defun enable-method-combinations (e4 e5)
  (import-functions-from-host
   '(sicl-method-combination:define-method-combination-expander
     sicl-loop::list-car sicl-loop::list-cdr)
   e5)
  (load-source-file "Method-combination/accessor-defgenerics.lisp" e5)
  (load-source-file "Method-combination/method-combination-template-defclass.lisp" e5)

  (with-intercepted-function-cells
      (e5
       (make-instance
        (env:function-cell (env:client e4) e4 'make-instance)))
    (load-source-file "Method-combination/find-method-combination.lisp" e5))
  (load-source-file "Method-combination/define-method-combination-defmacro.lisp" e5)
  (with-intercepted-function-cells
      (e5
       (make-instance
        (env:function-cell (env:client e4) e4 'make-instance)))
    (load-source-file "CLOS/standard-method-combination.lisp" e5))
  (load-source-file "CLOS/find-method-combination-defgenerics.lisp" e5)
  (load-source-file "CLOS/find-method-combination-defmethods.lisp" e5))
