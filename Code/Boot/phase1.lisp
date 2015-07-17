(cl:in-package #:sicl-boot)

;;;; The purpose of phase 1 is to create a class hierarchy that
;;;; mirrors the MOP class hierarchy.  The classes we create are all
;;;; host standard classes.  We create these classes by loading files
;;;; from the CLOS sub-directory that contain DEFCLASS forms defining
;;;; these classes.
;;;;
;;;; For compilation we use a compilation environment that contains
;;;; the normal definition of DEFCLASS, which expands to a call to
;;;; ENSURE-CLASS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating class accessor generic functions.
;;;
;;; There are different ways in which we can accomplish this task,
;;; given the constraint that it has to be done by loading DEFGENERIC
;;; forms corresponding to the class accessor generic functions.
;;;
;;; We obviously can not use the host definition of DEFGENERIC because
;;; it might clobber any existing host definition.  In particular,
;;; this is the case for class accessor functions that have names in
;;; the COMMON-LISP package, for instance CLASS-NAME.  Since we must
;;; supply our own definition of DEFGENERIC, we are free to do what we
;;; want.
;;;
;;; The way we have chosen to do it is to provide both a specific
;;; definition of DEFGENERIC and a specific definition of
;;; ENSURE-GENERIC-FUNCTION.  The difference between the standard
;;; version of DEFGENERIC is that it deletes any existing function
;;; associated with the name before creating a new one.  This way, we
;;; are sure that each time we run the bootstrapping process, we have
;;; a fresh generic function.  We do NOT delete any existing function
;;; in the special version of ENSURE-GENERIC-FUNCTION, because we want
;;; to use that special version to find the existing generic function
;;; when the class-initialization protocol needs to add a reader or a
;;; writer method on the generic function.  On the other hand, we do
;;; not want to use the ordinary SICL version of
;;; ENSURE-GENERIC-FUNCTION because it requires a battery of
;;; additional functionality in the form of other generic functions.
;;; So to keep things simple, we supply a special bootstrapping
;;; version of it.

;;; We define a special version of ENSURE-GENERIC-FUNCTION in the
;;; run-time environment to be used in phase 1.  This version of
;;; ENSURE-GENERIC-FUNCTION operates in ENV.  It checks whether there
;;; is already a function named FUNCTION-NAME in ENV.  If so that
;;; function is returned, and it is assumed to be a generic function.
;;; If not, an instance of the host class STANDARD-GENERIC-FUNCTION is
;;; created and associated with FUNCTION-NAME in ENV.
(defun define-ensure-generic-function-r1 (env)
  (setf (sicl-genv:fdefinition 'ensure-generic-function env)
	(lambda (function-name &rest arguments)
	  (let ((args (copy-list arguments)))
	    (loop while (remf args :environment))
	    (if (sicl-genv:fboundp function-name env)
		(sicl-genv:fdefinition function-name env)
		(setf (sicl-genv:fdefinition function-name env)
		      (apply #'make-instance 'standard-generic-function
			     :name function-name
			     args)))))))

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
    (define-ensure-generic-function-r1 r2)
    (define-defgeneric-phase1 r2)
    (ld "../CLOS/accessor-defgenerics.lisp" r2 r2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating MOP classes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Manual creation of some MOP classes.

;;; Since we do not use the host DEFCLASS macro nor the host version
;;; of ENSURE-CLASS in phase 1, our classes do not automatically have
;;; the host class named STANDARD-OBJECT as a superclass.  But being a
;;; subclass of STANDARD-OBJECT is a requirement for the host generic
;;; function INITIALIZE-INSTANCE to be able to initialize instances of
;;; a class.  We solve this problem by defining a special version of
;;; the class named T in phase 1 that in fact is the same as the host
;;; class STANDARD-OBJECT.  This way, we are sure that all our MOP
;;; classes in phase 1 are in fact subclass of the host class
;;; STANDARD-OBJECT.
(defun define-class-t-phase1 (env)
  (setf (sicl-genv:find-class 't env)
	(find-class 'standard-object)))

;;; We need a special definition of the class named FUNCTION in phase
;;; 1, because we want instances of this class to be funcallable in
;;; the host.  For that reason, we create this class as an instance of
;;; the host class FUNCALLABLE-STANDARD-CLASS.
(defun define-class-function-phase1 (env)
  (setf (sicl-genv:find-class 'function env)
	(make-instance 'closer-mop:funcallable-standard-class
	  :name (make-symbol (symbol-name '#:function)))))

(defun create-exceptional-mop-classes-phase1 (env)
  (define-class-t-phase1 env)
  (define-class-function-phase1 env))

(defun create-mop-classes-phase1 (boot)
  (create-exceptional-mop-classes-phase1 (r2 boot))
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
  (message "Start of phase 1~%")
  (create-class-accessor-generic-functions-phase1 boot)
  (create-mop-classes-phase1 boot)
  (message "End of phase 1~%"))

;;  LocalWords:  accessor
