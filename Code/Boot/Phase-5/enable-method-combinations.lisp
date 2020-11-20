(cl:in-package #:sicl-boot-phase-5)

(defun enable-method-combinations (e3 e5)
  (import-functions-from-host
   '(sicl-method-combination:define-method-combination-expander)
   e5)
  (load-source-file "Method-combination/accessor-defgenerics.lisp" e5)
  (with-intercepted-function-cells
      (e5
       (make-instance
           (list (lambda (name-or-class &rest initargs)
                   (format *trace-output* "IN MAKE-INSTANCE ~s~%" name-or-class)
                   (let ((class (if (symbolp name-or-class)
                                    (env:find-class (env:client e3) e3 name-or-class)
                                    name-or-class)))
                     (apply (env:fdefinition (env:client e3) e3 'make-instance)
                            class initargs))))))
    (load-source-file "Method-combination/method-combination-template-defclass.lisp" e5)
    (load-source-file "Method-combination/find-method-combination.lisp" e5)
    (load-source-file "Method-combination/define-method-combination-defmacro.lisp" e5)
    (load-source-file "CLOS/standard-method-combination.lisp" e5)))
