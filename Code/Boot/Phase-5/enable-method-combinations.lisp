(cl:in-package #:sicl-boot-phase-5)

(defun enable-method-combinations (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)) boot
    ;; FIND-METHOD-COMBINATION-TEMPLATE is called by FIND-METHOD-COMBINATION.
    (import-functions-from-host
     '(sicl-genv:find-method-combination-template
       (setf sicl-genv:find-method-combination-template)
       (setf sicl-genv:macro-function)
       null
       apply)
     e5)
    (import-functions-from-host
     '(gensym values rest second
       sicl-conditionals:cond-expander
       sicl-conditionals:and-expander)
     e4)
    (load-fasl "Method-combination/accessor-defgenerics.fasl" e5)
    ;; EQUAL is called by FIND-METHOD-COMBINATION in order to determine
    ;; equality between variant signatures.
    (import-function-from-host 'equal e5)
    (load-fasl "Method-combination/make-method-combination-defun.fasl" e5)
    (load-fasl "Method-combination/find-method-combination.fasl" e5)
    (import-function-from-host
     'sicl-method-combination::define-method-combination-expander e5)
    (load-fasl "Method-combination/define-method-combination-defmacro.fasl" e5)
    (load-fasl "Method-combination/method-combination-template-defclass.fasl" e4)
    ;; The standard method combination uses LOOP to traverse the list
    ;; of methods, so we need to import LIST-CAR and LIST-CDR from the
    ;; LOOP package.
    (import-functions-from-host '(sicl-loop::list-car sicl-loop::list-cdr) e5)
    ;; The standard method combination also uses REVERSE to reverse
    ;; the order of invocation of the :AFTER methods.
    (import-function-from-host 'reverse e5)
    (load-fasl "Conditionals/macros.fasl" e4)
    (load-fasl "CLOS/standard-method-combination.fasl" e5)
    (load-fasl "CLOS/find-method-combination-defgenerics.fasl" e5)
    (load-fasl "CLOS/find-method-combination-defmethods.fasl" e5)))
