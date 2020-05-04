(cl:in-package #:sicl-boot-phase-5)

(defun enable-allocate-instance (e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance e4)
        (lambda (class size)
          (make-instance 'sicl-boot::header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-fasl "CLOS/stamp-offset-defconstant.fasl" e4)
  (load-fasl "CLOS/allocate-instance-defgenerics.fasl" e4)
  (load-fasl "CLOS/allocate-instance-support.fasl" e4)
  (load-fasl "CLOS/allocate-instance-defmethods.fasl" e4))
