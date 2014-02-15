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
;;; Similarly, the METACLASS argument is a SYMBOL.  During phase 2 of
;;; the bootstrapping process, the metaclass is a host class, and
;;; MAKE-INSTANCE is that of the host, so we can just call
;;; MAKE-INSTANCE with the name of the metaclass.
;;; 
;;; Since this function is used only during bootstrapping, and
;;; bootstrapping is organized so that the this function will be
;;; called only when the class does not already exist, we do not have
;;; to check whether the class already exists, because we know that
;;; this is not the case.  
;;; 
;;; During phase 2 of the bootstrapping process, the superclasses (if
;;; any) can be found by using FIND-BRIDGE-CLASS.
(defun ensure-class (name
		     &rest arguments
		     &key
		       direct-superclasses
		       (metaclass nil metaclass-p)
		     &allow-other-keys)
  (let ((superclasses (loop for name in direct-superclasses
			    for class = (find-bridge-class name)
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
			 :name name
			 :direct-superclasses superclasses
			 remaining-keys)))
      (add-bridge-class name result)
      result)))
