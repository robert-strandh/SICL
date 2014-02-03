(cl:in-package #:sicl-clos)

(defmacro define-built-in-class (name
				 superclass-names
				 slot-specifiers
				 &rest options)
  `(ensure-built-in-class
    ',name
    :direct-superclasses 
    ,(canonicalize-direct-superclass-names superclass-names)
    :direct-slots
    ,(canonicalize-direct-slot-specs slot-specifiers)
    ,@(canonicalize-defclass-options options)))
