(cl:in-package #:sicl-boot-phase1)

;;; The macro DEFINE-BUILT-IN-CLASS expands to a call to this
;;; function, and this function is used only in that situation.  For
;;; that reason, we do not have to be particularly thorough about
;;; checking the validity of arguments.
;;;
;;; The argument DIRECT-SUPERCLASS-NAMES is a list of SYMBOLS.  Unlike
;;; the function ENSURE-CLASS we do not allow for class metaobject
;;; superclasses.
(defun ensure-built-in-class (name
			      &rest arguments
			      &key
				direct-default-initargs
				direct-superclasses
			      &allow-other-keys)
  ;; If the class already exists, then do nothing.
  (let ((class (find-bridge-class name nil)))
    (if (null class)
	(let ((superclasses (loop for name in direct-superclasses
				  for class = (find-bridge-class name)
				  collect class))
	      (remaining-keys (copy-list arguments)))
	  (loop while (remf remaining-keys :direct-superclasses))
	  (let ((result (apply #'cl:make-instance
			       'built-in-class
			       :direct-default-initargs direct-default-initargs
			       :name name
			       :direct-superclasses superclasses
			       remaining-keys)))
	    (add-bridge-class name result)
	    ;; FIXME: this is where we add create the accessors.
	    result))
	class)))
