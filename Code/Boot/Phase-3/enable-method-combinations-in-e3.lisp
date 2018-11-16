(cl:in-package #:sicl-boot-phase-3)

(defun enable-method-combinations-in-e3 (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) boot
    ;; FIND-METHOD-COMBINATION-TEMPLATE is called by FIND-METHOD-COMBINATION.
    (import-functions-from-host
     '(sicl-genv:find-method-combination-template
       (setf sicl-genv:find-method-combination-template))
     e3)
    (load-file "Method-combination/accessor-defgenerics.lisp" e3)
    ;; EQUAL is called by FIND-METHOD-COMBINATION in order to determine
    ;; equality between variant signatures.
    (import-function-from-host 'equal e3)
    (load-file "Method-combination/make-method-combination-defun.lisp" e3)
    (load-file "Method-combination/find-method-combination.lisp" e3)
    (import-function-from-host
     'sicl-method-combination::define-method-combination-expander e3)
    (load-file "Method-combination/define-method-combination-defmacro.lisp" e3)
    (load-file "Method-combination/method-combination-template-defclass.lisp" e2)
    ;; The standard method combination uses LOOP to traverse the list
    ;; of methods, so we need to import LIST-CAR and LIST-CDR from the
    ;; LOOP package.
    (import-functions-from-host '(sicl-loop::list-car sicl-loop::list-cdr) e3)
    ;; The standard method combination also uses REVERSE to reverse
    ;; the order of invocation of the :AFTER methods.
    (import-function-from-host 'reverse e3)
    (load-file "CLOS/standard-method-combination.lisp" e3)
    (load-file "CLOS/find-method-combination-defgenerics.lisp" e3)
    (load-file "CLOS/find-method-combination-defmethods.lisp" e3)))
