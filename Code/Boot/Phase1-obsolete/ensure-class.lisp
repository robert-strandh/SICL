(cl:in-package #:sicl-boot-phase1)

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
;;; Similarly, the METACLASS argument is a SYMBOL.  During phase 1 of
;;; the bootstrapping process, the metaclass must be a host class, and
;;; to make an instance, we must call CL:MAKE-INSTANCE.
;;; 
;;; During phase 1 of the bootstrapping process, the superclasses (if
;;; any) can be found by using FIND-BRIDGE-CLASS.
(defun ensure-class (name
		     &rest arguments
		     &key
		       direct-default-initargs
		       direct-slots
		       direct-superclasses
		       (metaclass nil metaclass-p)
		     &allow-other-keys)
  ;; If the class already exists, then do nothing.
  (let ((class (find-bridge-class name nil)))
    (if (null class)
	(let ((superclasses (loop for name in direct-superclasses
				  for class = (find-bridge-class name)
				  collect class))
	      (remaining-keys (copy-list arguments)))
	  (loop while (remf remaining-keys :metaclass))
	  (loop while (remf remaining-keys :direct-superclasses))
	  (let* ((class (if metaclass-p
			    metaclass
			    'standard-class))
		 (result (apply #'make-instance class
				:direct-default-initargs direct-default-initargs
				:direct-slots direct-slots
				:name name
				:direct-superclasses superclasses
				remaining-keys)))
	    (add-bridge-class name result)
	    result))
	class)))
