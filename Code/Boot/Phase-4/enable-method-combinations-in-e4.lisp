(cl:in-package #:sicl-new-boot-phase-4)

(defun enable-method-combinations-in-e4 (boot)
  (with-accessors ((e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)) boot
    ;; FIND-METHOD-COMBINATION-TEMPLATE is called by FIND-METHOD-COMBINATION.
    (import-functions-from-host
     '(sicl-genv:find-method-combination-template
       (setf sicl-genv:find-method-combination-template))
     e4)
    (load-file "Method-combination/accessor-defgenerics.lisp" e4)
    ;; EQUAL is called by FIND-METHOD-COMBINATION in order to determine
    ;; equality between variant signatures.
    (import-function-from-host 'equal e4)
    (load-file "Method-combination/make-method-combination-defun.lisp" e4)
    (load-file "Method-combination/find-method-combination.lisp" e4)
    (import-function-from-host
     'sicl-method-combination::define-method-combination-expander e4)
    (load-file "Method-combination/define-method-combination-defmacro.lisp" e4)
    (load-file "Method-combination/method-combination-template-defclass.lisp" e3)
    ;; The standard method combination uses LOOP to traverse the list
    ;; of methods, so we need to import LIST-CAR and LIST-CDR from the
    ;; LOOP package.
    (import-functions-from-host '(sicl-loop::list-car sicl-loop::list-cdr) e4)
    ;; The standard method combination also uses REVERSE to reverse
    ;; the order of invocation of the :AFTER methods.
    (import-function-from-host 'reverse e4)
    (load-file "CLOS/standard-method-combination.lisp" e4)
    (load-file "CLOS/find-method-combination-defgenerics.lisp" e4)
    (load-file "CLOS/find-method-combination-defmethods.lisp" e4)))
