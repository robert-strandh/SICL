(cl:in-package #:sicl-clos)

;;; The DEFCLASS macro.  The AMOP is inconsistent with respect to the
;;; CLHS.  For instance, it requires the arguments to ENSURE-CLASS to
;;; appear in the same order as they appear in the DEFCLASS form, but
;;; that should not matter since they are not evaluated.  Furthermore,
;;; the AMOP talks about additional class options, but no such
;;; additional options are permitted according to the CLHS.  We follow
;;; the CLHS.

;;; The AMOP says that the NAME argument to DEFCLASS becomes the first
;;; argument to ENSURE-CLASS.  Nothing particular here.
;;;
;;; The AMOP says that the DIRECT-SUPERCLASSES argument to DEFCLASS
;;; becomes the value of the :DIRECT-SUPERCLASSES argument to
;;; ENSURE-CLASS.  The CLHS requires that the DIRECT-SUPERCLASSES
;;; argument to DEFCLASS be a proper list of non-NIL symbols.

(defmacro defclass (name
		    superclass-names
		    slot-specifiers
		    &rest options)
  `(ensure-class ',name
		 :name ',name
		 :direct-superclasses 
		 ,(canonicalize-direct-superclass-names superclass-names)
		 :direct-slots
		 ,(canonicalize-direct-slot-specs slot-specifiers)
		 ,@(canonicalize-defclass-options options)))
