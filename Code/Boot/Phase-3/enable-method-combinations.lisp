(cl:in-package #:sicl-boot-phase-3)

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
        (env:function-cell (env:client e2) e2 'make-instance))
       (sicl-method-combination:find-method-combination-template
        (env:function-cell
         (env:client e4) e4 'sicl-method-combination:find-method-combination-template)))
    (load-source-file "Method-combination/find-method-combination.lisp" e3))
  (load-source-file "Method-combination/define-method-combination-defmacro.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (make-instance
        (env:function-cell (env:client e2) e2 'make-instance))
       ((setf sicl-method-combination:find-method-combination-template)
        (env:function-cell
         (env:client e4) e4
         '(setf sicl-method-combination:find-method-combination-template))))
    (load-source-file "CLOS/standard-method-combination.lisp" e3))
  (load-source-file "CLOS/find-method-combination-defgenerics.lisp" e3)
  (load-source-file "CLOS/find-method-combination-defmethods.lisp" e3))
