(cl:in-package #:sicl-new-boot-phase-3)

(defun enable-method-combinations-in-e3 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (import-function-from-host 'sicl-genv:find-method-combination-template e3)
    (import-function-from-host '(setf sicl-genv:find-method-combination-template) e3)
    (import-function-from-host 'equal e3)
    (load-file "Method-combination/accessor-defgenerics.lisp" e3)
    (load-file "Method-combination/find-method-combination.lisp" e3)
    (import-function-from-host 'sicl-method-combination::lambda-list-variables e3)
    (import-function-from-host 'sicl-method-combination::wrap-body e3)
    (import-function-from-host 'cleavir-code-utilities:separate-function-body e3)
    (load-file "Method-combination/long-form-expansion.lisp" e3)
    (import-functions-from-host
     '(sicl-loop::list-car sicl-loop::list-cdr
       nth reverse
       gethash (setf gethash) make-hash-table)
     e3)
    (load-file "Method-combination/short-form-expansion.lisp" e3)
    (load-file "Method-combination/define-method-combination-support.lisp" e3)
    (load-file "Method-combination/define-method-combination-defmacro.lisp" e3)
    (load-file "Method-combination/method-combination-template-defclass.lisp" e2)
    (load-file "CLOS/standard-method-combination.lisp" e3)
    (load-file "CLOS/find-method-combination-defgenerics.lisp" e3)
    (load-file "CLOS/find-method-combination-defmethods.lisp" e3)))

