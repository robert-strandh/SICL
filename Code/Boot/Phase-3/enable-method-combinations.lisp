(cl:in-package #:sicl-boot-phase-3)

(defun enable-method-combinations (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) boot
    (load-fasl "Method-combination/accessor-defgenerics.fasl" e3)
    (load-fasl "Method-combination/make-method-combination-defun.fasl" e3)
    (load-fasl "Method-combination/find-method-combination.fasl" e3)
    (load-fasl "Method-combination/method-combination-template-defclass.fasl" e2)
    (load-fasl "CLOS/standard-method-combination.fasl" e3)
    (load-fasl "CLOS/find-method-combination-defgenerics.fasl" e3)
    (load-fasl "CLOS/find-method-combination-defmethods.fasl" e3)))
