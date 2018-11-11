(cl:in-package #:sicl-new-boot-phase-2)

(defclass header (closer-mop:funcallable-standard-object)
  ((%class :initarg :class)
   (%rack :initarg :rack))
  (:metaclass closer-mop:funcallable-standard-class))

(defun enable-allocate-instance-in-e2 (e2)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance e2)
        (lambda (class size)
          (make-instance 'header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-access e2)
        (lambda (object location)
          (aref (slot-value object '%rack) location)))
  (setf (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e2)
        (lambda (value object location)
          (setf (aref (slot-value object '%rack) location) value)))
  (import-functions-from-host
   '((setf sicl-genv:constant-variable) sort assoc every
     mapc 1+ 1- subseq butlast position identity nthcdr equal
     remove-if-not mapcar reverse find compile)
   e2)
  (load-file "CLOS/class-unique-number-offset-defconstant.lisp" e2)
  (load-file "CLOS/allocate-instance-defgenerics.lisp" e2)
  (load-file "CLOS/allocate-instance-support.lisp" e2)
  (load-file "CLOS/allocate-instance-defmethods.lisp" e2))
