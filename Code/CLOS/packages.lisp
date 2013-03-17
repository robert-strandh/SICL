(cl:in-package #:common-lisp-user)

(defpackage #:sicl-common-lisp
  (:nicknames #:scl)
  (:export . #.(let ((symbols '()))
		 (do-external-symbols (symbol '#:common-lisp)
		   (push (symbol-name symbol) symbols))
		 symbols))
  (:shadowing-import-from
   #:common-lisp
   ;; Lambda list keywords.  The have no definitions associated with
   ;; them, so it doesn't matter what home package they have.
   #:&optional #:&rest #:&body #:&key #:&allow-other-keys
   #:&aux #:&whole #:&environment
   ;; Symbols that have to do with declarations and that have
   ;; no other meaning, or at least no meaning that we are currently
   ;; interested in.
   #:declare #:inline #:notinline #:ignore #:ignorable #:type #:ftype
   #:optimize #:debug #:speed #:compilation-speed #:space #:safety))

(defpackage #:sicl-clos
  (:use #:sicl-additional-conditions
	#:sicl-code-utilities
	#:sicl-common-lisp)
  (:export
   ;; MOP classes
   #:class #:standard-class #:built-in-class #:structure-class
   #:standard-object #:function
   #:generic-function #:standard-generic-function
   #:method #:standard-method
   #:method-combination
   #:slot-definition
   #:direct-slot-definition
   #:effective-slot-definition
   #:standard--slot-definition
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   ;; Functions related to slot definitions.
   #:slot-definition-name
   #:slot-definition-allocation
   #:slot-definition-type
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-location
   #:make-direct-slot-definition
   #:make-effective-slot-definition
   #:compute-effective-slot-definition
   ;;
   #:defclass #:find-class
   #:class-name #:class-of
   #:allocate-instance #:make-instance
   #:initialize-instance #:shared-initialize #:reinitialize-instance
   #:make-instances-obsolete
   #:update-instance-for-different-class
   #:update-instance-for-redefined-class
   #:method #:standard-method
   #:make-method #:add-method #:call-method #:find-method
   #:method-qualifiers #:compute-applicable-methods #:next-method-p
   #:invalid-method-error
   #:no-applicable-method #:no-next-method #:remove-method #:defmethod
   #:method-combination #:define-method-combination #:method-combination-error
   #:make-instance
   ;; Generic functions.
   #:generic-function #:standard-generic-function
   #:defgeneric #:ensure-generic-function
   #:slot-value #:slot-missing #:slot-boundp #:slot-makunbound
   #:print-object #:describe-object #:documentation
   ;; SICL-specific macro
   #:define-built-in-class))

(defpackage #:sicl-conditionals
  (:use #:sicl-common-lisp))

(defpackage #:sicl-cons-high
  (:use #:sicl-common-lisp))
