(cl:in-package #:sicl-boot-phase-7)

;;; FIXME: use a version of LOAD-FASL that creates machine code.
(defun load-make-instance (e5)
  (sicl-boot:load-source "CLOS/make-instance-support.lisp" e5)
  (sicl-boot:load-source "CLOS/make-instance-defgenerics.lisp" e5)
  (sicl-boot:load-source "CLOS/make-instance-defmethods.lisp" e5))
