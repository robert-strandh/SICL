(cl:in-package #:sicl-boot-phase-3)

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
           (list (lambda (class-or-name &rest initargs)
                   (let ((class (if (symbolp class-or-name)
                                    (env:find-class (env:client e3) e3 class-or-name)
                                    class-or-name)))
                     (apply (env:fdefinition (env:client e3) e3 'make-instance)
                            class initargs))))))
    (load-source-file "Method-combination/find-method-combination.lisp" e4))
  (load-source-file "Method-combination/define-method-combination-defmacro.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (make-instance
           (list (lambda (class-or-name &rest initargs)
                   (let ((class (if (symbolp class-or-name)
                                    (env:find-class (env:client e3) e3 class-or-name)
                                    class-or-name)))
                     (apply (env:fdefinition (env:client e3) e3 'make-instance)
                            class initargs)))))
       (find-class (list (lambda (x) (env:find-class (env:client e3) e3 x)))))
    (load-source-file "CLOS/standard-method-combination.lisp" e4))
  (setf (env:fdefinition (env:client e5) e5 'sicl-clos:find-method-combination)
        (lambda (generic-function name arguments)
          (declare (ignore generic-function))
          (funcall (env:fdefinition
                    (env:client e4) e4 'sicl-method-combination:find-method-combination)
                   name arguments))))
