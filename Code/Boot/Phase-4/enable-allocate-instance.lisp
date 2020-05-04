(cl:in-package #:sicl-boot-phase-4)

(defun enable-allocate-instance (e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance e3)
        (lambda (class size)
          (make-instance 'sicl-boot::header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-fasl "CLOS/stamp-offset-defconstant.fasl" e3)
  (load-fasl "CLOS/allocate-instance-defgenerics.fasl" e3)
  (load-fasl "CLOS/allocate-instance-support.fasl" e3)
  (load-fasl "CLOS/allocate-instance-defmethods.fasl" e3))
