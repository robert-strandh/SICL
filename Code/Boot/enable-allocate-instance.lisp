(cl:in-package #:sicl-boot)

(defun enable-allocate-instance (ea)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance ea)
        (lambda (class size)
          (make-instance 'sicl-boot::header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-source "CLOS/stamp-offset-defconstant.lisp" ea)
  (load-source "CLOS/allocate-instance-defgenerics.lisp" ea)
  (load-source "CLOS/allocate-instance-support.lisp" ea)
  (load-source "CLOS/allocate-instance-defmethods.lisp" ea))
