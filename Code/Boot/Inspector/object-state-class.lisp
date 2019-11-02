(cl:in-package #:sicl-boot-inspector)

;;; This is the base class for all the state classes on ersatz object.
(defclass inspected-ersatz-object
    (clouseau::inspected-identity-object-mixin
     clouseau::inspected-object)
  ())

;;; Impure objects.

(defclass inspected-impure-object (inspected-ersatz-object)
  ())

(defclass inspected-impure-eql-specializer (inspected-impure-object)
  ())

(defclass inspected-impure-standard-class (inspected-impure-object)
  ())

(defclass inspected-impure-funcallable-standard-class
    (inspected-impure-object)
  ())

(defclass inspected-impure-built-in-class
    (inspected-impure-object)
  ())

(defclass inspected-impure-function
    (inspected-impure-object)
  ())

(defclass inspected-impure-standard-generic-function
    (inspected-impure-function)
  ())

(defclass inspected-impure-standard-method
    (inspected-impure-object)
  ())

(defclass inspected-impure-standard-reader-method
    (inspected-impure-standard-method)
  ())

(defclass inspected-impure-standard-writer-method
    (inspected-impure-standard-method)
  ())

(defclass inspected-impure-standard-slot-definition
    (inspected-impure-object)
  ())

(defclass inspected-impure-standard-direct-slot-definition
    (inspected-impure-slot-definition)
  ())

(defclass inspected-impure-standard-effective-slot-definition
    (inspected-impure-slot-definition)
  ())

;;; Pure objects.

(defclass inspected-pure-object (inspected-ersatz-object)
  ())

(defclass inspected-pure-eql-specializer (inspected-pure-object)
  ())

(defclass inspected-pure-standard-class (inspected-pure-object)
  ())

(defclass inspected-pure-funcallable-standard-class
    (inspected-pure-object)
  ())

(defclass inspected-pure-built-in-class
    (inspected-pure-object)
  ())

(defclass inspected-pure-function
    (inspected-pure-object)
  ())

(defclass inspected-pure-standard-generic-function
    (inspected-pure-function)
  ())

(defclass inspected-pure-standard-method
    (inspected-pure-object)
  ())

(defclass inspected-pure-standard-reader-method
    (inspected-pure-standard-method)
  ())

(defclass inspected-pure-standard-writer-method
    (inspected-pure-standard-method)
  ())

(defclass inspected-pure-standard-slot-definition
    (inspected-pure-object)
  ())

(defclass inspected-pure-standard-direct-slot-definition
    (inspected-pure-slot-definition)
  ())

(defclass inspected-pure-standard-effective-slot-definition
    (inspected-pure-slot-definition)
  ())

;;; Very pure objects.

(defclass inspected-very-pure-object (inspected-ersatz-object)
  ())

(defclass inspected-very-pure-eql-specializer (inspected-very-pure-object)
  ())

(defclass inspected-very-pure-standard-class (inspected-very-pure-object)
  ())

(defclass inspected-very-pure-funcallable-standard-class
    (inspected-very-pure-object)
  ())

(defclass inspected-very-pure-built-in-class
    (inspected-very-pure-object)
  ())

(defclass inspected-very-pure-function
    (inspected-very-pure-object)
  ())

(defclass inspected-very-pure-standard-generic-function
    (inspected-very-pure-function)
  ())

(defclass inspected-very-pure-standard-method
    (inspected-very-pure-object)
  ())

(defclass inspected-very-pure-standard-reader-method
    (inspected-very-pure-standard-method)
  ())

(defclass inspected-very-pure-standard-writer-method
    (inspected-very-pure-standard-method)
  ())

(defclass inspected-very-pure-standard-slot-definition
    (inspected-very-pure-object)
  ())

(defclass inspected-very-pure-standard-direct-slot-definition
    (inspected-very-pure-slot-definition)
  ())

(defclass inspected-very-pure-standard-effective-slot-definition
    (inspected-very-pure-slot-definition)
  ())

(defgeneric object-state-class-with-class (object class))

(defgeneric object-state-class-with-class-and-metaclass (object class metaclass))

(defmethod clouseau:object-state-class
    ((object sicl-boot-phase-3::header)
     (place t))
  (object-state-class-with-class object (class-of-object object)))

;;; This method is applicable for impure objects, so the class is found
;;; in E3.
(defmethod object-state-class-with-class (object class)
  (declare (ignore object))
  (case (funcall (sicl-genv:fdefinition 'class-name (sicl-boot::e3 *boot*)) class)
    (sicl-clos:eql-specializer 'inspected-impure-eql-specializer)
    (standard-class 'inspected-impure-standard-class)
    (sicl-clos:funcallable-standard-class 'inspected-impure-funcallable-standard-class)
    (built-in-class 'inspected-impure-built-in-class)
    (standard-generic-function 'inspected-impure-standard-generic-function)
    (function 'inspected-impure-function)
    (sicl-clos:standard-reader-method 'inspected-impure-standard-reader-method)
    (sicl-clos:standard-writer-method 'inspected-impure-standard-writer-method)
    (standard-method 'inspected-impure-standard-method)
    (sicl-clos:standard-direct-slot-definition 'inspected-impure-standard-direct-slot-definition)
    (sicl-clos:standard-effective-slot-definition 'inspected-impure-standard-effective-slot-definition)
    (t 'clouseau:inspected-object)))
    
;;; This method is applicable for all pure objects.
(defmethod object-state-class-with-class (object (class sicl-boot-phase-3::header))
  (object-state-class-with-class-and-metaclass object class (class-of-object class)))

;;; This method is applicable for pure objects, so the class is found
;;; in E4.
(defmethod object-state-class-with-class-and-metaclass (object class metaclass)
  (declare (ignore object))
  (case (funcall (sicl-genv:fdefinition 'class-name (sicl-boot::e4 *boot*)) class)
    (sicl-clos:eql-specializer 'inspected-pure-eql-specializer)
    (standard-class 'inspected-pure-standard-class)
    (sicl-clos:funcallable-standard-class 'inspected-pure-funcallable-standard-class)
    (built-in-class 'inspected-pure-built-in-class)
    (standard-generic-function 'inspected-pure-standard-generic-function)
    (function 'inspected-pure-function)
    (sicl-clos:standard-reader-method 'inspected-pure-standard-reader-method)
    (sicl-clos:standard-writer-method 'inspected-pure-standard-writer-method)
    (standard-method 'inspected-pure-standard-method)
    (sicl-clos:standard-direct-slot-definition 'inspected-pure-standard-direct-slot-definition)
    (sicl-clos:standard-effective-slot-definition 'inspected-pure-standard-effective-slot-definition)
    (t 'clouseau:inspected-object)))
  
;;; This method is applicable for very pure objects, so the class is
;;; found in E5.
(defmethod object-state-class-with-class-and-metaclass
    (object class (metaclass sicl-boot-phase-3::header))
  (declare (ignore object))
  (case (funcall (sicl-genv:fdefinition 'class-name (sicl-boot::e5 *boot*)) class)
    (sicl-clos:eql-specializer 'inspected-very-pure-eql-specializer)
    (standard-class 'inspected-very-pure-standard-class)
    (sicl-clos:funcallable-standard-class 'inspected-very-pure-funcallable-standard-class)
    (built-in-class 'inspected-very-pure-built-in-class)
    (standard-generic-function 'inspected-very-pure-standard-generic-function)
    (function 'inspected-very-pure-function)
    (sicl-clos:standard-reader-method 'inspected-very-pure-standard-reader-method)
    (sicl-clos:standard-writer-method 'inspected-very-pure-standard-writer-method)
    (standard-method 'inspected-very-pure-standard-method)
    (sicl-clos:standard-direct-slot-definition 'inspected-very-pure-standard-direct-slot-definition)
    (sicl-clos:standard-effective-slot-definition 'inspected-very-pure-standard-effective-slot-definition)
    (t 'clouseau:inspected-object)))
