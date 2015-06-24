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
  (let ((c1 (c1 boot))
	(r1 (r1 boot))
	(r2 (r1 boot)))
    (ld "../CLOS/accessor-defgenerics.lisp" c1 r2)
    (ld "../CLOS/standard-object-defclass.lisp" c1 r1)
    (ld "../CLOS/metaobject-defclass.lisp" c1 r1)
    (ld "../CLOS/method-defclass.lisp" c1 r1)
    (ld "../CLOS/standard-method-defclass.lisp" c1 r1)
    (ld "../CLOS/standard-accessor-method-defclass.lisp" c1 r1)
    (ld "../CLOS/standard-reader-method-defclass.lisp" c1 r1)
    (ld "../CLOS/standard-writer-method-defclass.lisp" c1 r1)
    (ld "../CLOS/slot-definition-defclass.lisp" c1 r1)
    (ld "../CLOS/standard-slot-definition-defclass.lisp" c1 r1)
    (ld "../CLOS/direct-slot-definition-defclass.lisp" c1 r1)
    (ld "../CLOS/effective-slot-definition-defclass.lisp" c1 r1)
    (ld "../CLOS/standard-direct-slot-definition-defclass.lisp" c1 r1)
    (ld "../CLOS/standard-effective-slot-definition-defclass.lisp" c1 r1)
    (ld "../CLOS/specializer-defclass.lisp" c1 r1)
    (ld "../CLOS/eql-specializer-defclass.lisp" c1 r1)
    (ld "../CLOS/class-unique-number-defparameter.lisp" c1 r1)
    (ld "../CLOS/class-defclass.lisp" c1 r1)
    (ld "../CLOS/forward-referenced-class-defclass.lisp" c1 r1)
    (ld "../CLOS/real-class-defclass.lisp" c1 r1)
    (ld "../CLOS/regular-class-defclass.lisp" c1 r1)
    (ld "../CLOS/standard-class-defclass.lisp" c1 r1)
    (ld "../CLOS/funcallable-standard-class-defclass.lisp" c1 r1)
    (ld "../CLOS/built-in-class-defclass.lisp" c1 r1)
    (ld "function-temporary-defclass.lisp" c1 r1)
    (ld "../CLOS/funcallable-standard-object-defclass.lisp" c1 r1)
    (ld "../CLOS/generic-function-defclass.lisp" c1 r1)
    (ld "../CLOS/standard-generic-function-defclass.lisp" c1 r1)))
