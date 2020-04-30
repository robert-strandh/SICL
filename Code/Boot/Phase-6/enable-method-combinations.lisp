(cl:in-package #:sicl-boot-phase-6)

(defun enable-method-combinations (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (load-fasl "Method-combination/accessor-defgenerics.fasl" e6)
    (load-fasl "Method-combination/make-method-combination-defun.fasl" e6)
    (load-fasl "Method-combination/find-method-combination.fasl" e6)
    (load-fasl "Method-combination/method-combination-template-defclass.fasl" e5)
    (load-fasl "Conditionals/macros.fasl" e5)
    (load-fasl "CLOS/standard-method-combination.fasl" e6)
    (load-fasl "CLOS/find-method-combination-defgenerics.fasl" e6)
    (load-fasl "CLOS/find-method-combination-defmethods.fasl" e6)))
