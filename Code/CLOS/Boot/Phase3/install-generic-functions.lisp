(cl:in-package #:sicl-clos)

(defparameter *installable-generic-functions*
  '(;; These are used by the :AFTER method on initialize-instance
    ;; specialized for STANDARD-CLASS.
    class-direct-subclasses
    (setf c-direct-subclasses)
    class-direct-superclasses
    (setf c-direct-superclasses)
    class-direct-slots
    (setf c-direct-slots)
    ;; These are used by the :AFTER method on initialize-instance,
    ;; specialized for STANDARD-GENERIC-FUNCTION.
    (setf gf-documentation)
    (setf gf-declarations)
    (setf gf-method-class)
    (setf specializer-profile)
    (setf gf-lambda-list)
    (setf gf-argument-precedence-order)
    (setf discriminating-function)
    (setf gf-name)
    ;; These are used by the :AFTER method on initialize-instance,
    ;; specialized for STANDARD-METHOD
    (setf method-documentation)
    ))

(loop for name in *installable-generic-functions*
      do (setf (fdefinition name)
	       (cdr (assoc name *bridge-generic-functions* :test #'equal)))
	 (setf *bridge-generic-functions*
	       (remove name *bridge-generic-functions*
		       :key #'car :test #'equal)))
