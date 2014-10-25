(cl:in-package #:common-lisp-user)

(defpackage #:sicl-global-environment
  (:nicknames #:sicl-env)
  (:use #:common-lisp)
  ;; When this package is defined in a host implementation for the
  ;; purpose of cross compilation, we shadow the symbols of the
  ;; COMMON-LISP package that name functions and variables that have
  ;; to do with manipulating the environment.
  (:shadow #:fboundp
	   #:fdefinition
	   #:macro-function
	   #:compiler-macro-function
	   #:boundp
	   #:makunbound
	   #:fmakunbound
	   #:find-class
	   )
  (:export
   #:environment
   #:fboundp
   #:fdefinition
   #:macro-function
   #:compiler-macro-function
   #:function-type
   #:function-inline
   #:constant-variable
   #:special-variable
   #:symbol-macro
   #:variable-type
   #:setf-expander
   #:type-expander
   #:boundp
   #:makunbound
   #:fmakunbound
   #:find-class
   #:special-operator
   ))

