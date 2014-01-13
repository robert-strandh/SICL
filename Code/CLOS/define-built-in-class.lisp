(cl:in-package #:sicl-clos)

(defmacro define-built-in-class (name superclass-names slot-specifiers)
  `(ensure-class ',name
		 :direct-superclasses 
		 ,(canonicalize-direct-superclass-names superclass-names)
		 :direct-slots
		 ,(canonicalize-direct-slot-specs slot-specifiers)
		 :metaclass
		 'built-in-class))
