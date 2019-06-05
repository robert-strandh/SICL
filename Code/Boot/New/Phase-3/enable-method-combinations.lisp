(cl:in-package #:sicl-boot-phase-3)

(defun enable-method-combinations (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) boot
    (flet ((ld (name environment)
             (format *trace-output* "Loading file ~s~%" name)
             (load-fasl name environment)))
      ;; FIND-METHOD-COMBINATION-TEMPLATE is called by FIND-METHOD-COMBINATION.
      (import-function-from-host 'sicl-genv:find-method-combination-template e3)
      (import-function-from-host '(setf sicl-genv:find-method-combination-template) e3)
      (ld "Method-combination/accessor-defgenerics.fasl" e3)
      ;; EQUAL is called by FIND-METHOD-COMBINATION in order to determine
      ;; equality between variant signatures.
      (import-function-from-host 'equal e3)
      (ld "Method-combination/make-method-combination-defun.fasl" e3)
      (ld "Method-combination/find-method-combination.fasl" e3)
      (ld "Method-combination/method-combination-template-defclass.fasl" e2)
      ;; The standard method combination uses LOOP to traverse the list
      ;; of methods, so we need to import LIST-CAR and LIST-CDR from the
      ;; LOOP package.
      (import-function-from-host 'sicl-loop::list-car e3)
      (import-function-from-host 'sicl-loop::list-cdr e3)
      ;; The standard method combination also uses REVERSE to reverse
      ;; the order of invocation of the :AFTER methods.
      (import-function-from-host 'reverse e3)
      (ld "CLOS/standard-method-combination.fasl" e3)
      (ld "CLOS/find-method-combination-defgenerics.fasl" e3)
      (ld "CLOS/find-method-combination-defmethods.fasl" e3))))
