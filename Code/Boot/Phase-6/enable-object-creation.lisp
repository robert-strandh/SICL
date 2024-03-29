(cl:in-package #:sicl-boot-phase-6)

(defun enable-object-initialization (e5)
  (load-source-file "CLOS/instance-slots-offset-defconstant.lisp" e5)
  (load-source-file "CLOS/shared-initialize-support.lisp" e5)
  (load-source-file "CLOS/shared-initialize-defgenerics.lisp" e5)
  (load-source-file "CLOS/shared-initialize-defmethods.lisp" e5)
  (load-source-file "CLOS/initialize-instance-defgenerics.lisp" e5)
  (load-source-file "CLOS/initialize-instance-defmethods.lisp" e5))

(defun enable-object-allocation (e5)
  (setf (env:fdefinition (env:client e5) e5 'sicl-clos::allocate-general-instance)
        (lambda (class size)
          (make-instance 'sicl-boot:header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-source-file "CLOS/stamp-offset-defconstant.lisp" e5)
  (load-source-file "CLOS/effective-slot-definition-class.lisp" e5)
  (load-source-file "CLOS/allocate-instance-support.lisp" e5)
  (load-source-file "CLOS/allocate-instance-defgenerics.lisp" e5)
  (load-source-file "CLOS/allocate-instance-defmethods.lisp" e5))

(defun enable-class-finalization (e5)
  (load-source-file "CLOS/class-finalization-defgenerics.lisp" e5)
  (load-source-file "CLOS/class-finalization-support.lisp" e5)
  (load-source-file "CLOS/class-finalization-defmethods.lisp" e5))

(defun enable-make-instance (e5)
  (load-source-file "CLOS/make-instance-defgenerics.lisp" e5)
  (load-source-file "CLOS/make-instance-defmethods.lisp" e5))

(defun enable-slot-value (e5)
  (setf (env:constant-variable
         (env:client e5) e5 'sicl-clos::+unbound-slot-value+)
        10000000)
  (load-source-file "CLOS/standard-instance-access.lisp" e5)
  (load-source-file "CLOS/slot-bound-using-index.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-defgenerics.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-support.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-defmethods.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-specified-defuns.lisp" e5))

(defun enable-object-creation (e5)
  (enable-slot-value e5)
  (enable-object-initialization e5)
  (enable-object-allocation e5)
  (enable-class-finalization e5)
  (enable-make-instance e5))
