(cl:in-package #:sicl-new-boot-phase-3)

(defun enable-method-combinations-in-e3 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (import-function-from-host 'equal e3)
    (load-file "Method-combination/accessor-defgenerics.lisp" e3)
    ;; FIND-METHOD-COMBINATION-TEMPLATE is called by FIND-METHOD-COMBINATION.
    (import-function-from-host 'sicl-genv:find-method-combination-template e3)
    (load-file "Method-combination/find-method-combination.lisp" e3)
    (import-function-from-host '(setf sicl-genv:find-method-combination-template) e3)
    (import-functions-from-host
     '(sicl-loop::list-car sicl-loop::list-cdr
       nth reverse)
      e3)
    (import-function-from-host
     'sicl-method-combination::define-method-combination-expander e3)
    (load-file "Method-combination/define-method-combination-defmacro.lisp" e3)
    (load-file "Method-combination/method-combination-template-defclass.lisp" e2)
    (load-file "CLOS/standard-method-combination.lisp" e3)
    (load-file "CLOS/find-method-combination-defgenerics.lisp" e3)
    (load-file "CLOS/find-method-combination-defmethods.lisp" e3)))

