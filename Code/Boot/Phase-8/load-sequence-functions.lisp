(cl:in-package #:sicl-boot-phase-8)

(defun load-sequence-functions (e5)
  (load-source "Array/array-total-size-limit-defconstant.lisp" e5)
  (load-source "Sequence/types.lisp" e5)
  (load-source "Sequence/with-cons-iterator.lisp" e5))
