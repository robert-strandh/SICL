(cl:in-package #:sicl-boot-phase-3)

(defun enable-method-combinations (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) boot
    (flet ((ld (name environment)
             (load-fasl name environment)))
      (ld "Method-combination/accessor-defgenerics.fasl" e3)
      (ld "Method-combination/make-method-combination-defun.fasl" e3)
      (ld "Method-combination/find-method-combination.fasl" e3)
      (ld "Method-combination/method-combination-template-defclass.fasl" e2)
      (ld "CLOS/standard-method-combination.fasl" e3)
      (ld "CLOS/find-method-combination-defgenerics.fasl" e3)
      (ld "CLOS/find-method-combination-defmethods.fasl" e3))))
