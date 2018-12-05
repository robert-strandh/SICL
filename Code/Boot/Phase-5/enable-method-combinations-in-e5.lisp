(cl:in-package #:sicl-boot-phase-5)

(defun enable-method-combinations-in-e5 (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)) boot
    ;; FIND-METHOD-COMBINATION-TEMPLATE is called by FIND-METHOD-COMBINATION.
    (import-functions-from-host
     '(sicl-genv:find-method-combination-template
       (setf sicl-genv:find-method-combination-template))
     e5)
    (load-file "Method-combination/accessor-defgenerics.lisp" e5)
    ;; EQUAL is called by FIND-METHOD-COMBINATION in order to determine
    ;; equality between variant signatures.
    (import-function-from-host 'equal e5)
    (load-file "Method-combination/make-method-combination-defun.lisp" e5)
    (load-file "Method-combination/find-method-combination.lisp" e5)
    (import-function-from-host
     'sicl-method-combination::define-method-combination-expander e5)
    (load-file "Method-combination/define-method-combination-defmacro.lisp" e5)
    (load-file "Method-combination/method-combination-template-defclass.lisp" e4)
    ;; The standard method combination uses LOOP to traverse the list
    ;; of methods, so we need to import LIST-CAR and LIST-CDR from the
    ;; LOOP package.
    (import-functions-from-host '(sicl-loop::list-car sicl-loop::list-cdr) e5)
    ;; The standard method combination also uses REVERSE to reverse
    ;; the order of invocation of the :AFTER methods.
    (import-function-from-host 'reverse e5)
    (load-file "CLOS/standard-method-combination.lisp" e5)
    (load-file "CLOS/find-method-combination-defgenerics.lisp" e5)
    (load-file "CLOS/find-method-combination-defmethods.lisp" e5)))
