(cl:in-package #:sicl-clos)

;;; The macro DEFCLASS expands to a call to this function, and, during
;;; bootstrapping, this function is used only in that situation.  For
;;; that reason, we do not have to be particularly thorough about
;;; checking the validity of arguments.
;;;
;;; The argument DIRECT-SUPERCLASSES is a list of SYMBOLS.  Unlike
;;; the normal function ENSURE-CLASS (i.e. the one used when
;;; bootstrapping is done) we do not allow for class metaobject
;;; superclasses.
;;;
;;; Similarly, the METACLASS argument is a SYMBOL.  During phase 3 of
;;; the bootstrapping process, the metaclass is a bridge class, and
;;; MAKE-INSTANCE is our own function that is able to instantiate
;;; bridge classes, taking as an argument either the class or its
;;; name, so we can just call MAKE-INSTANCE with the name of the
;;; metaclass.
;;; 
;;; Since this function is used only during bootstrapping, and
;;; bootstrapping is organized so that the this function will be
;;; called only when the class does not already exist, we do not have
;;; to check whether the class already exists, because we know that
;;; this is not the case.  
;;; 
;;; During phase 3 of the bootstrapping process, the superclasses (if
;;; any) can be found by using FIND-TARGET-CLASS.

;;; Since at this point ENSURE-CLASS is currently fbound to the
;;; version used in phase 2, we start by making it funbound so as to
;;; avoid compiler warnings.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'ensure-class))

(defun ensure-class (name
		     &rest arguments
		     &key
		       direct-default-initargs
		       direct-slots
		       direct-superclasses
		       (metaclass nil metaclass-p)
		     &allow-other-keys)
  (let ((superclasses (loop for name in direct-superclasses
			    for class = (find-target-class name)
			    do (when (null class)
				 ;; This should not happen during
				 ;; bootstrapping.
				 (error "unknown class ~s" name))
			    collect class))
	(remaining-keys (copy-list arguments)))
    (loop while (remf remaining-keys :metaclass))
    (loop while (remf remaining-keys :direct-superclasses))
    (let* ((class (if metaclass-p metaclass 'standard-class))
	   (result (apply #'make-instance class
			  :direct-default-initargs direct-default-initargs
			  :direct-slots direct-slots
			  :name name
			  :direct-superclasses superclasses
			  remaining-keys)))
      (add-target-class name result)
      result)))
