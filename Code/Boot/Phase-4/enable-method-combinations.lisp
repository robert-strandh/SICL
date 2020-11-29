(cl:in-package #:sicl-boot-phase-4)

(defun enable-method-combinations (e3 e4 e5)
  (import-functions-from-host
   '(sicl-method-combination:define-method-combination-expander
     sicl-loop::list-car sicl-loop::list-cdr)
   e4)
  (load-source-file "Method-combination/accessor-defgenerics.lisp" e4)
  (load-source-file "Method-combination/method-combination-template-defclass.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (make-instance
        (env:function-cell (env:client e3) e3 'make-instance))
       (sicl-method-combination:find-method-combination-template
        (env:function-cell
         (env:client e5) e5 'sicl-method-combination:find-method-combination-template)))
    (load-source-file "Method-combination/find-method-combination.lisp" e4))
  (load-source-file "Method-combination/define-method-combination-defmacro.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (make-instance
        (env:function-cell (env:client e3) e3 'make-instance))
       ((setf sicl-method-combination:find-method-combination-template)
        (env:function-cell
         (env:client e5) e5
         '(setf sicl-method-combination:find-method-combination-template))))
    (load-source-file "CLOS/standard-method-combination.lisp" e4))
  (load-source-file "CLOS/find-method-combination-defgenerics.lisp" e4)
  (load-source-file "CLOS/find-method-combination-defmethods.lisp" e4))
