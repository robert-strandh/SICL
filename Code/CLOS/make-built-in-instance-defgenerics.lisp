(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAKE-BUILT-IN-INSTANCE.
;;;
;;; This generic function is analogous to MAKE-INSTANCE, but instead
;;; of taking instances of STANDARD-CLASS, it takes instances of
;;; BUILT-IN-CLASS.
;;;
;;; In a way similar to that of MAKE-INSTANCE, it has two preexisting
;;; methods, one specialized to BUILT-IN-CLASS and another specialized
;;; to SYMBOL.  The latter method looks up the class named by the
;;; symbol and calls MAKE-BUILT-IN-INSTANCE recursively with that
;;; class.

(defgeneric make-built-in-instance (class &rest initargs))
