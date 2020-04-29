(cl:in-package #:sicl-boot-phase-4)

(defun enable-method-combinations (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)) boot
    (load-fasl "Method-combination/accessor-defgenerics.fasl" e4)
    (load-fasl "Method-combination/make-method-combination-defun.fasl" e4)
    (load-fasl "Method-combination/find-method-combination.fasl" e4)
    (load-fasl "Method-combination/define-method-combination-defmacro.fasl" e4)
    (load-fasl "Method-combination/method-combination-template-defclass.fasl" e3)
    (load-fasl "Conditionals/macros.fasl" e3)
    (load-fasl "CLOS/standard-method-combination.fasl" e4)
    (load-fasl "CLOS/find-method-combination-defgenerics.fasl" e4)
    (load-fasl "CLOS/find-method-combination-defmethods.fasl" e4)))
