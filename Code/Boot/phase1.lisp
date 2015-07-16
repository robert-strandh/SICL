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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating class accessor generic functions.

;;; Define the macro DEFGENERIC for use in phase 1.  We define it a
;;; bit differently from its usual definition.  Its main purpose is to
;;; define a generic function in the environment ENV.  However, before
;;; definining it, we remove the existing generic function if it
;;; exists.  This way, we are sure to get a fresh generic function, as
;;; opposed to one that happened to have been imported from the host.
;;; We must, of course, make sure that we execute a DEFGENERIC form
;;; for a particular generic function exactly once, but we can do that
;;; because we completely master the boot process.
(defun define-defgeneric-phase1 (env)
  (setf (sicl-genv:macro-function 'defgeneric env)
	(lambda (form environment)
	  (declare (ignore environment))
	  `(progn (sicl-genv:fmakunbound ',(second form) ,env)
		  (setf (sicl-genv:fdefinition ',(second form) ,env)
			(ensure-generic-function
			 ',(second form)
			 :name ',(second form)
			 :lambda-list ',(third form)))))))

(defun create-class-accessor-generic-functions-phase1 (boot)
  (let ((r2 (r2 boot)))
    (define-defgeneric-phase1 r2)
    (ld "../CLOS/accessor-defgenerics.lisp" r2 r2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating MOP classes.

(defun create-mop-classes-phase1 (boot)
  ;; (ld "../CLOS/standard-object-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/metaobject-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/method-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/standard-method-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/standard-accessor-method-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/standard-reader-method-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/standard-writer-method-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/slot-definition-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/standard-slot-definition-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/direct-slot-definition-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/effective-slot-definition-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/standard-direct-slot-definition-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/standard-effective-slot-definition-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/specializer-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/eql-specializer-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/class-unique-number-defparameter.lisp" c1 r1)
  ;; (ld "../CLOS/class-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/forward-referenced-class-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/real-class-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/regular-class-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/standard-class-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/funcallable-standard-class-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/built-in-class-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/funcallable-standard-object-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/generic-function-defclass.lisp" c1 r1)
  ;; (ld "../CLOS/standard-generic-function-defclass.lisp" c1 r1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point of phase 1.

(defun phase1 (boot)
  (create-class-accessor-generic-functions-phase1 boot)
  (create-mop-classes-phase1 boot))
