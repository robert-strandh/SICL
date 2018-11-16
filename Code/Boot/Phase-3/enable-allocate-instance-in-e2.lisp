(cl:in-package #:sicl-boot-phase-3)

(defun enable-allocate-instance-in-e2 (e2)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance e2)
        (lambda (class size)
          (make-instance 'sicl-boot-phase-2::header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (import-functions-from-host
   '(sort every
     mapc 1+ 1- subseq butlast position identity nthcdr equal
     remove-if-not mapcar reverse find)
   e2)
  (import-function-from-host '(setf sicl-genv:constant-variable) e2)
  (load-file "CLOS/class-unique-number-offset-defconstant.lisp" e2)
  (load-file "CLOS/allocate-instance-defgenerics.lisp" e2)
  (load-file "CLOS/allocate-instance-support.lisp" e2)
  (load-file "CLOS/allocate-instance-defmethods.lisp" e2))
