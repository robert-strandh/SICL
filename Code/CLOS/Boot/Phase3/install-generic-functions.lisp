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
    class-direct-default-initargs
    (setf c-direct-default-initargs)
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
    ;; Generic functions for initialization
    initialize-instance
    reinitialize-instance
    shared-initialize
    initialize-built-in-instance
    ))

(loop for name in *installable-generic-functions*
      do (setf (fdefinition name)
	       (find-bridge-generic-function name))
	 (delete-bridge-generic-function name))
