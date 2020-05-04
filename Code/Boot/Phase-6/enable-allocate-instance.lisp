(cl:in-package #:sicl-boot-phase-6)

(defun enable-allocate-instance (e5)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance e5)
        (lambda (class size)
          (make-instance 'sicl-boot::header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-fasl "CLOS/stamp-offset-defconstant.fasl" e5)
  (load-fasl "CLOS/allocate-instance-defgenerics.fasl" e5)
  (load-fasl "CLOS/allocate-instance-support.fasl" e5)
  (load-fasl "CLOS/allocate-instance-defmethods.fasl" e5))
