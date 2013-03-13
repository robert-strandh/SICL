(asdf:operate 'asdf:load-op :sicl-code-utilities)
(asdf:operate 'asdf:load-op :sicl-additional-conditions)

(load "packages.lisp")

(in-package #:sicl-clos)

(cl:defmacro with-temporary-functions (bindings &body body)
  (let ((vars (loop for binding in bindings collect (gensym)))
	(unbound-indicator (gensym)))
    `(let ,(loop for (name definition) in bindings
		 for var in vars
		 collect `(,var
			   (if (fboundp ',name)
			       (fdefinition ',name)
			       ',unbound-indicator)))
       ,@(loop for (name definition) in bindings
	       collect `(setf (fdefinition ',name) ,definition))
       ,@body
       ,@(loop for (name definition) in bindings
	       for var in vars
	       collect `(if (eq ,var ',unbound-indicator)
			    (fmakunbound ',name)
			    (setf (fdefinition ',name) ,var))))))

(defun nopfun (&rest args)
  (declare (ignore args))
  nil)

;;; Initially, we define DEFCLASS to be a synonym for CL:DEFCLASS,
;;; with the difference being that we do not consider metaclasses
;;; other than the default, and that the superclass T is replaced
;;; by the superclass TT.
(defmacro defclass (name superclasses slots &rest class-options)
  `(cl:defclass ,name
       ,(substitute 'tt 't superclasses)
     ,slots
     ,@(remove :metaclass class-options :key #'car)))

;;; We can't define a class called T, and it is a bad idea to shadow
;;; CL:T, so we call it TT instead for now.
(defclass tt () ())

(defclass function () ())

(defmacro defgeneric (&rest args)
  `(cl:defgeneric ,@args))

(defmacro defmethod (&rest args)
  `(cl:defmethod ,@args))

(with-temporary-functions
    ((make-instance #'cl:make-instance)
     (find-class #'cl:find-class))
  ;; Load the file containing DEFCLASS forms defining the MOP
  ;; hierarchy of classes.
  (load "mop-class-hierarchy.lisp")
  (load "slot-definition.lisp"))

;;; Define FIND-CLASS and (SETF FIND-CLASS). 
(load "class-database.lisp")

;;; Define ENSURE-CLASS, ENSURE-CLASS-USING-CLASS, and DEFCLASS
(fmakunbound 'classp)
(load "defclass.lisp")

(with-temporary-functions
    ((make-instance #'cl:make-instance)
     (ensure-class
      (lambda (name &rest args &key metaclass &allow-other-keys)
	(apply #'ensure-class-using-class
	       nil
	       name
	       :metaclass (cl:find-class (if (null metaclass)
					     'standard-class
					     metaclass))
	       args)))
     ;; Another problem is our definition of CLASSP, which was
     ;; designed for target metaobjects, but right now, CLASSP needs
     ;; to return true for just about anything (except symbol).
     (classp
      (lambda (object)
	(not (symbolp object))))
     (initialize-instance #'cl:initialize-instance)
     (add-reader-method #'nopfun)
     (add-writer-method #'nopfun))
  (load "class-initialization.lisp")
  ;; We can't use DEFCLASS to create built-in classes, so we
  ;; create them "manually" by using MAKE-INSTANCE.
  (setf (find-class 't)
	(cl:make-instance 'built-in-class
			  :name 't
			  :direct-superclasses '()))
  (setf (find-class 'function)
	(cl:make-instance 'built-in-class
			  :name 'function
			  :direct-superclasses '()))
  ;; Load the file containing DEFCLASS forms defining the MOP
  ;; hierarchy of classes.
  (load "mop-class-hierarchy.lisp")
  (load "slot-definition.lisp"))

;;; At this point, we have all the bridge classes we need.

;;; Now we need class finalization because soon we are going to want
;;; to allocate instances of our bridge classes, and to do that, they
;;; have to be finalized.  The finalization protocol will be
;;; implemented as host generic functions that take bridge classes as
;;; arguments.
;;; Apparently, we can't play the usual tricks with
;;; call-next-method.  We need to temporarily unintern it, and
;;; then shadow it again later.
(unintern 'call-next-method)
(load "class-finalization.lisp")
(shadow '#:call-next-method)

;;; Let us start the finalization by computing the precedence list of
;;; the built-in bridge classes we have created so far, namely T and
;;; FUNCTION.
(setf (cl:slot-value (find-class 't) '%precedence-list)
      (compute-class-precedence-list (find-class 't)))

(setf (cl:slot-value (find-class 'function) '%precedence-list)
      (compute-class-precedence-list (find-class 'function)))

;;; During class finalization of our bridge classes, we are going to
;;; allocate effective slot definitions.  This is done by calling
;;; MAKE-INSTANCE on the class returned by
;;; EFFECTIVE-SLOT-DEFINITION-CLASS.  Right now,
;;; EFFECTIVE-SLOT-DEFINITION-CLASS returns a bridge class, which we
;;; don't want because we want the effective slot definitions to be
;;; host instances.  Furthermore we need for MAKE-INSTANCE to mean
;;; CL:MAKE-INSTANCE.
(with-temporary-functions
    ((make-instance #'cl:make-instance)
     (effective-slot-definition-class
      (lambda (class)
	(declare (ignore class))
	(cl:find-class 'standard-effective-slot-definition))))
  (maphash (lambda (name bridge-class)
	     (declare (ignore name))
	     (unless (class-finalized-p bridge-class)
	       (finalize-inheritance bridge-class)))
	   *classes*))

(load "class-unique-number.lisp")

;;; To allocate target instances of our bridge classes, we need
;;; MAKE-INSTANCE of course, but that one is easy.  But MAKE-INSTANCE
;;; calls INITIALIZE-INSTANCE and indirectly SHARED-INITIALIZE.  Those
;;; two are generic functions that will be called with target
;;; instances.  So we need generic functions that can specialize for
;;; bridge classes.  This will be our next task.

(load "generic-function.lisp")
