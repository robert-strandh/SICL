(in-package #:sicl-utilities)

;;; In order to avoid problems that might be difficult to avoid,
;;; we systematically make variables unbound when a file is loaded. 
;;; This macro is used to accomplish that.
(defmacro defunbound (variable)
  `(progn (defvar ,variable)
	  (eval-when (:compile-toplevel :load-toplevel :execute)
	    (makunbound ',variable))))

;;; We also don't want some global variables to have one value during
;;; one part of the initialization process and another value later
;;; because it was incorrectly assigned a second time, so we use this
;;; macro to signal an error if we attempt to assign a variable that
;;; is already bound.
(defmacro setunbound (variable value)
  `(if (boundp ',variable)
       (error "attempt to assign to a variable that already has a value")
       (setf ,variable ,value)))
