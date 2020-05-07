(cl:in-package #:sicl-boot-phase-7)

;;; FIXME: use a version of LOAD-FASL that creates machine code.
(defun load-make-instance (e5)
  (sicl-boot:load-fasl "CLOS/make-instance-support.fasl" e5)
  (sicl-boot:load-fasl "CLOS/make-instance-defgenerics.fasl" e5)
  (sicl-boot:load-fasl "CLOS/make-instance-defmethods.fasl" e5))
