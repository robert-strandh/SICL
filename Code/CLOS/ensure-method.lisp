(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD.
;;;
;;; This function is not required by the AMOP to exist, 
;;;
;;; One can think of this function as a convenience function that
;;; combines a call to MAKE-INSTANCE with a call to ADD-METHOD.  A
;;; SPECIALIZER, can be either a specializer meta object, or a symbol.
;;; If it is a symbol, it is considered to be the name of a class, and
;;; this function replaces the symbol with the corresponding class
;;; before calling MAKE-INSTANCE.
;;;
;;; Since we might need to find the class with a particular name, we
;;; apply the general rule in SICL, namely to use explicit
;;; environments whenever we can.  Since this function is not part of
;;; any specification, we are free to do what we want, so we add the
;;; environment as a required parameter.  When this function is used
;;; in the expansion of DEFMETHOD, the environment supplied as the
;;; second argument is the run-time environment (and not the
;;; compile-time environment) in which the call to DEFMETHOD is made.
;;;
;;; Since the environment might be a lexical environment or a global
;;; environment, we use the function SICL-ENVIRONMENT:FIND-CLASS for
;;; the lookup, because that function automatically finds the global
;;; environment if it is given a lexical environment.

(defun ensure-method (generic-function
		      environment
		      &rest keys
		      &key specializers
		      &allow-other-keys)
  ;; FIXME: Check that SPECIALIZERS is a proper list, and that
  ;; it contains only symbols and specializer metaobjects. 
  (let ((specs (loop for s in specializers
		     collect (if (symbolp s)
				 (sicl-genv:find-class s environment)
				 s)))
	(remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :specializers))
    (let ((method (apply #'make-instance
			 (generic-function-method-class generic-function)
			 :specializers specs
			 remaining-keys)))
      (add-method generic-function method)
      method)))
