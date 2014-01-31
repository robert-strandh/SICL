(cl:in-package #:sicl-clos)

(defmethod ensure-class-using-class
    ((class null)
     name
     &rest keys
     &key
       direct-superclasses
       metaclass
       &allow-other-keys)
  (declare (ignore direct-superclasses metaclass))
  (apply #'ensure-class-using-class-null name keys))

(defmethod ensure-class-using-class
    ((class class)
     name
     &rest keys
     &key
       direct-superclasses
       metaclass
       &allow-other-keys)
  (declare (ignore direct-superclasses metaclass))
  (apply #'ensure-class-using-class-class class name keys))

(defmethod ensure-class-using-class
    ((class forward-referenced-class)
     name
     &rest keys
     &key
       direct-superclasses
       metaclass
     &allow-other-keys)
  (apply #'ensure-class-using-class-forward-referenced-class class name keys))
