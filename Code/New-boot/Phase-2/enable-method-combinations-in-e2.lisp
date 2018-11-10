(cl:in-package #:sicl-new-boot-phase-2)

(defun enable-method-combination-in-e2 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)) boot
    ;; FIND-METHOD-COMBINATION-TEMPLATE is called by FIND-METHOD-COMBINATION.
    (import-functions-from-host
     '(sicl-genv:find-method-combination-template
       (setf sicl-genv:find-method-combination-template))
     e2)
    (load-file "Method-combination/accessor-defgenerics.lisp" e2)
    ;; EQUAL is called by FIND-METHOD-COMBINATION in order to determine
    ;; equality between variant signatures.
    (import-function-from-host 'equal e2)
    (load-file "Method-combination/make-method-combination-defun.lisp" e2)
    (load-file "Method-combination/find-method-combination.lisp" e2)
    (import-function-from-host
     'sicl-method-combination::define-method-combination-expander e2)
    (load-file "Method-combination/define-method-combination-defmacro.lisp" e2)
    (load-file "Method-combination/method-combination-template-defclass.lisp" e1)
    ;; The standard method combination uses LOOP to traverse the list
    ;; of methods, so we need to import LIST-CAR and LIST-CDR from the
    ;; LOOP package.
    (import-functions-from-host '(sicl-loop::list-car sicl-loop::list-cdr) e2)
    ;; The standard method combination also uses REVERSE to reverse
    ;; the order of invocation of the :AFTER methods.
    (import-function-from-host 'reverse e2)
    (load-file "CLOS/standard-method-combination.lisp" e2)
    (load-file "CLOS/find-method-combination-defgenerics.lisp" e2)
    (load-file "CLOS/find-method-combination-defmethods.lisp" e2)))
