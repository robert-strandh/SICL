(cl:in-package #:sicl-type)

(defgeneric typep-atomic (object type environment))

(defmethod typep-atomic (object type environment)
  (let* ((global-environment (sicl-genv:global-environment environment))
	 (expander (sicl-genv:type-expander type global-environment))
	 (type-class (sicl-genv:find-class type global-environment))
	 (object-class (class-of object)))
    (cond ((not (null expander))
	   ;; We found an expander.  Expand the type call TYPEP
	   ;; recursively with the expanded type.
	   (generic-typep object (funcall expander type) global-environment))
	  ((not (null type-class))
	   ;; The type specifier is the name of a class.  Then return
	   ;; true if and only if the class of the object is a
	   ;; subclass of the class named by the type.
	   (closer-mop:subclassp object-class type-class))
	  (t
	   ;; The type has no expander associated with it and the type
	   ;; is not also a class.  Furthermore, there was no method
	   ;; on TYPEP-ATOMIC specialized to the name of the type.  
	   ;; This can only mean that TYPE is not a valid type.
	   (error "unknown type ~s" type)))))

(defmethod typep-atomic (object (type (eql 'atom)) environment)
  (atom object))

(defmethod typep-atomic (object (type (eql 'base-char)) environment)
  (characterp object))

(defmethod typep-atomic (object (type (eql 'standard-char)) environment)
  (characterp object))

(defmethod typep-atomic (object (type (eql 'character)) environment)
  (characterp object))

(defun typep-atomic-float (object type)
  (and (floatp object)
       (same-float-type-p (type-of object) type)))

(defmethod typep-atomic (object (type (eql 'short-float)) environment)
  (typep-atomic-float object type))

(defmethod typep-atomic (object (type (eql 'single-float)) environment)
  (typep-atomic-float object type))

(defmethod typep-atomic (object (type (eql 'double-float)) environment)
  (typep-atomic-float object type))

(defmethod typep-atomic (object (type (eql 'long-float)) environment)
  (typep-atomic-float object type))

(defmethod typep-atomic (object (type (eql 'keyword)) environment)
  (and (symbolp object)
       (eq (symbol-package object)
	   (find-package '#:keyword))))

(defmethod typep-atomic (object (type (eql 'nil)) environment)
  nil)

(defmethod typep-atomic (object (type (eql 'simple-array)) environment)
  (and (arrayp object)
       (null (array-displacement object))))
