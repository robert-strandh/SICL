(cl:in-package #:sicl-boot-phase-7)

(defclass client (sicl-boot:client) ())

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let ((sicl-client:*client* (make-instance 'client)))
      (load-source-file "Arithmetic/realp-defgeneric.lisp" e5)
      (load-source-file "Arithmetic/rationalp-defgeneric.lisp" e5)
      (ensure-asdf-system '#:sicl-arithmetic-defuns e5)
      (load-source-file "Cons/accessor-defuns.lisp" e5)
      (ensure-asdf-system '#:sicl-cons-defuns e5))))
