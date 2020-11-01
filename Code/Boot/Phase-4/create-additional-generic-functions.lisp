(cl:in-package #:sicl-boot-phase-4)

(defun create-additional-generic-functions (e5)
  (load-source-file "Arithmetic/convert-fixnum-to-bignum-defun.lisp" e5)
  (load-source-file "Arithmetic/binary-add-defgeneric.lisp" e5)
  (load-source-file "Arithmetic/binary-add-defmethods.lisp" e5))
