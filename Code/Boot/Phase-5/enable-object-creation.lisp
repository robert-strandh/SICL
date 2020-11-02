(cl:in-package #:sicl-boot-phase-5)

(defun enable-object-creation (e5)
  (setf (env:fdefinition (env:client e5) e5 'sicl-clos::allocate-general-instance)
        (lambda (class size)
          (make-instance 'sicl-boot:header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-source-file "CLOS/stamp-offset-defconstant.lisp" e5)
  (load-source-file "CLOS/effective-slot-definition-class-support.lisp" e5)
  (load-source-file "CLOS/effective-slot-definition-class-defgeneric.lisp" e5)
  (load-source-file "CLOS/effective-slot-definition-class-defmethods.lisp" e5))
