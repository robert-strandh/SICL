(cl:in-package #:sicl-boot-phase-5)

(defun prepare-next-phase (e3 e4 e5)
  (load-source-file "CLOS/class-of-defun.lisp" e5)
  (enable-typep e3 e5)
  (load-source-file "Types/type-of-defgeneric.lisp" e5)
  (enable-object-creation e3 e5))
