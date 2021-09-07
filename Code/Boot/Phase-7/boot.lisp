(cl:in-package #:sicl-boot-phase-7)

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (load-asdf-system '#:sicl-arithmetic-defuns e5)
    (load-source-file "Cons/accessor-defuns.lisp" e5)
    (load-asdf-system '#:sicl-cons-defuns e5)))
