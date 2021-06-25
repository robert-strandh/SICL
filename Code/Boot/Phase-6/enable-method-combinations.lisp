(cl:in-package #:sicl-boot-phase-6)

(defun import-functions-from-host-into-e5
    (names e3 e5)
  (loop with e3-client = (env:client e3)
        with make-instance = (env:fdefinition e3-client e3 'make-instance)
        with e5-client = (env:client e5)
        for name in names
        for host-function = (fdefinition name)
        for sicl-function = (funcall make-instance 'sicl-clos:simple-function)
        do (setf (sicl-boot:original-function sicl-function) host-function)
           (sicl-host-mop:set-funcallable-instance-function
            sicl-function host-function)
           (setf (env:fdefinition e5-client e5 name) sicl-function)))

(defun enable-method-combinations (e3 e5)
  (import-functions-from-host-into-e5
   '(;; sicl-method-combination:define-method-combination-expander
     sicl-method-combination::lambda-list-variables
     sicl-loop::list-car sicl-loop::list-cdr)
   e3 e5)
  (load-source-file "Method-combination/method-group-specifier.lisp" e5)
  (load-source-file "Method-combination/method-discriminator.lisp" e5)
  (load-source-file "Method-combination/long-form-expansion.lisp" e5)
  (load-source-file "Method-combination/short-form-expansion.lisp" e5)
  (load-source-file "Method-combination/define-method-combination-support.lisp" e5)
  (load-source-file "Method-combination/accessor-defgenerics.lisp" e5)
  (load-source-file "Method-combination/method-combination-template-defclass.lisp" e5)
  (load-source-file "Method-combination/find-method-combination.lisp" e5)
  (load-source-file "Method-combination/define-method-combination-defmacro.lisp" e5)
  (load-source-file "CLOS/standard-method-combination.lisp" e5)
  (load-source-file "CLOS/find-method-combination-defgenerics.lisp" e5)
  (load-source-file "CLOS/find-method-combination-defmethods.lisp" e5))
