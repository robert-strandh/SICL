(cl:in-package #:sicl-boot)

;;;; The purpose of phase 1 is to create in the run-time environment
;;;; number 1 a class hierarchy that mirrors the MOP class hierarchy.
;;;; The classes we create are all host standard classes.  We create
;;;; these classes by loading files from the CLOS sub-directory that
;;;; contain DEFCLASS forms defining these classes.
;;;;
;;;; For compilation we use compilation environment number 1.  This
;;;; environment contains the normal definition of DEFCLASS, which
;;;; expands to a call to ENSURE-CLASS.

(defun phase1 (boot)
  (let ((c (c1 boot))
	(r (r1 boot)))
    (define-class-prototype r)
    (ld "ensure-class-temporary-defun.lisp" c r)
    (ld "../CLOS/standard-object-defclass.lisp" c r)
    (ld "../CLOS/metaobject-defclass.lisp" c r)
    (ld "../CLOS/method-defclass.lisp" c r)
    (ld "../CLOS/standard-method-defclass.lisp" c r)
    (ld "../CLOS/standard-accessor-method-defclass.lisp" c r)
    (ld "../CLOS/standard-reader-method-defclass.lisp" c r)
    (ld "../CLOS/standard-writer-method-defclass.lisp" c r)
    (ld "../CLOS/slot-definition-defclass.lisp" c r)
    (ld "../CLOS/standard-slot-definition-defclass.lisp" c r)
    (ld "../CLOS/direct-slot-definition-defclass.lisp" c r)
    (ld "../CLOS/effective-slot-definition-defclass.lisp" c r)
    (ld "../CLOS/standard-direct-slot-definition-defclass.lisp" c r)
    (ld "../CLOS/standard-effective-slot-definition-defclass.lisp" c r)
    (ld "../CLOS/specializer-defclass.lisp" c r)
    (ld "../CLOS/eql-specializer-defclass.lisp" c r)
    (ld "../CLOS/class-unique-number-defparameter.lisp" c r)
    (ld "../CLOS/class-defclass.lisp" c r)
    (ld "../CLOS/forward-referenced-class-defclass.lisp" c r)
    (ld "../CLOS/real-class-defclass.lisp" c r)
    (ld "../CLOS/regular-class-defclass.lisp" c r)
    (ld "../CLOS/standard-class-defclass.lisp" c r)
    (ld "../CLOS/funcallable-standard-class-defclass.lisp" c r)
    (ld "../CLOS/built-in-class-defclass.lisp" c r)
    (ld "function-temporary-defclass.lisp" c r)
    (ld "../CLOS/funcallable-standard-object-defclass.lisp" c r)
    (ld "../CLOS/generic-function-defclass.lisp" c r)
    (ld "../CLOS/standard-generic-function-defclass.lisp" c r)))
