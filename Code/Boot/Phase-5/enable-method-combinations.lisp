(cl:in-package #:sicl-boot-phase-5)

(defun enable-method-combinations (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)) boot
    (load-fasl "Method-combination/accessor-defgenerics.fasl" e5)
    (load-fasl "Method-combination/make-method-combination-defun.fasl" e5)
    (load-fasl "Method-combination/find-method-combination.fasl" e5)
    (load-fasl "Method-combination/method-combination-template-defclass.fasl" e4)
    (load-fasl "CLOS/standard-method-combination.fasl" e5)
    (load-fasl "CLOS/find-method-combination-defgenerics.fasl" e5)
    (load-fasl "CLOS/find-method-combination-defmethods.fasl" e5)))
