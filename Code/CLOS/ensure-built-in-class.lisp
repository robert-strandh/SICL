(cl:in-package #:sicl-clos)

;;; The macro DEFINE-BUILT-IN-CLASS expands to a call to this
;;; function, and this function is used only in that situation.  For
;;; that reason, we do not have to be particularly thorough about
;;; checking the validity of arguments.
;;;
;;; The argument DIRECT-SUPERCLASS-NAMES is a list of SYMBOLS.  Unlike
;;; the function ENSURE-CLASS we do not allow for class class
;;; metaobject superclasses.
(defun ensure-built-in-class (name
			      &rest arguments
			      &key
				direct-superclass-names
			      &allow-other-keys)
  ;; If the class already exists, then do nothing.
  (when (null (find-class name nil))
    (let ((superclasses (loop for name in direct-superclass-names
			      for class = (find-class name)
			      do (when (null class)
				   (error "unknown class ~s" name))
			      collect class))
	  (remaining-keys (copy-list arguments)))
      (loop while (remf remaining-keys :direct-superclasses))
      (let ((result (apply #'make-instance 'built-in-class
			   :name name
			   :direct-superclasses superclasses
			   remaining-keys)))
	(setf (find-class name) result)
	;; Since we require for superclasses to exist, and since we
	;; don't allow for built-in classes to be redefined, we can
	;; finalize the inheritance immediately. 
	(finalize-inheritance-aux result)
	result))))

	
      
