(cl:in-package #:sicl-boot-phase-6)

(defun enable-allocate-instance (e5)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance e5)
        (lambda (class size)
          (make-instance 'sicl-boot-phase-3::header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (import-functions-from-host
   '(sort every
     mapc 1+ 1- subseq butlast position identity nthcdr equal
     remove-if-not reverse find)
   e5)
  (import-function-from-host '(setf sicl-genv:constant-variable) e5)
  (load-fasl "CLOS/class-unique-number-offset-defconstant.fasl" e5)
  (load-fasl "CLOS/allocate-instance-defgenerics.fasl" e5)
  (load-fasl "CLOS/allocate-instance-support.fasl" e5)
  (load-fasl "CLOS/allocate-instance-defmethods.fasl" e5))
