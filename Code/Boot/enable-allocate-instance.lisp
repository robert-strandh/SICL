(cl:in-package #:sicl-boot)

(defun enable-allocate-instance (ea)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance ea)
        (lambda (class size)
          (make-instance 'sicl-boot::header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-fasl "CLOS/stamp-offset-defconstant.fasl" ea)
  (load-fasl "CLOS/allocate-instance-defgenerics.fasl" ea)
  (load-fasl "CLOS/allocate-instance-support.fasl" ea)
  (load-fasl "CLOS/allocate-instance-defmethods.fasl" ea))
