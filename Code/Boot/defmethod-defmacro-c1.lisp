(cl:in-package #:sicl-clos)

;;;; This definition of the macro DEFMETHOD is less general than the
;;;; normal definition.  It is only guaranteed to work when the
;;;; generic function is of type STANDARD-GENERIC-FUNCTION, and the
;;;; method to be created can be of type STANDARD-METHOD.
;;;;
;;;; The advantage of this definition is that it does not require the
;;;; full generality of MAKE-METHOD-LAMBDA.  Instead, it uses the
;;;; default version of MAKE-METHOD-LAMBDA.
;;;;
;;;; By using this version, we avoid calling CLASS-PROTOTYPE and
;;;; GENERIC-FUNCTION-METHOD-CLASS.  These two functions pose a
;;;; problem because if we were to call them here, they would have to
;;;; be the functions defined by the host.  However, when we define
;;;; the accessor generic functions for the MOP classes in environment
;;;; R1, we also define those two functions to be the target versions,
;;;; and the two versions can not exist simultaneously.  We avoid the
;;;; problem by not needing the host versions of these functions.

(defmacro defmethod (function-name &rest rest)
  (multiple-value-bind
	(qualifiers lambda-list specializers declarations documentation forms)
      (parse-defmethod rest)
    `(temporary-ensure-method
      ',function-name
      ',lambda-list
      ',qualifiers
      ,(canonicalize-specializers specializers)
      ,documentation
      ,(sicl-clos::make-method-lambda-default
	nil nil
	`(lambda ,lambda-list
	   ,@declarations
	   ,@forms)
	nil))))
