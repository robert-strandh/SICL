(cl:in-package #:sicl-boot-phase-6)

(defun enable-method-combinations (e5)
  (import-functions-from-host
   '(sicl-method-combination:define-method-combination-expander)
   e5)
  (load-source-file "Method-combination/accessor-defgenerics.lisp" e5)
  (load-source-file "Method-combination/method-combination-template-defclass.lisp" e5)
  (load-source-file "Method-combination/find-method-combination.lisp" e5)
  (load-source-file "Method-combination/define-method-combination-defmacro.lisp" e5)
  (load-source-file "CLOS/standard-method-combination.lisp" e5))
