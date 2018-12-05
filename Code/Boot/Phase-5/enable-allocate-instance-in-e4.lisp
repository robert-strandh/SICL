(cl:in-package #:sicl-boot-phase-5)

(defclass header (closer-mop:funcallable-standard-object)
  ((%class :initarg :class)
   (%rack :initarg :rack))
  (:metaclass closer-mop:funcallable-standard-class))

(defun enable-allocate-instance-in-e4 (e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance e4)
        (lambda (class size)
          (make-instance 'sicl-boot-phase-2::header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (import-functions-from-host
   '(sort every
     mapc 1+ 1- subseq butlast position identity nthcdr equal
     remove-if-not mapcar reverse find)
   e4)
  (import-function-from-host '(setf sicl-genv:constant-variable) e4)
  (load-file "CLOS/class-unique-number-offset-defconstant.lisp" e4)
  (load-file "CLOS/allocate-instance-defgenerics.lisp" e4)
  (load-file "CLOS/allocate-instance-support.lisp" e4)
  (load-file "CLOS/allocate-instance-defmethods.lisp" e4))
