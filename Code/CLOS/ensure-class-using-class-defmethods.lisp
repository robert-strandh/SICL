(cl:in-package #:sicl-clos)

(defmethod ensure-class-using-class
    ((class null)
     name
     &rest keys
     &key
       direct-default-initargs
       direct-slots
       direct-superclasses
       metaclass
       &allow-other-keys)
  (declare (ignore direct-default-initargs
                   direct-slots
                   direct-superclasses
                   metaclass))
  (apply #'ensure-class-using-class-null class name keys))

(defmethod ensure-class-using-class
    ((class class)
     name
     &rest keys
     &key
       direct-default-initargs
       direct-slots
       direct-superclasses
       metaclass
       &allow-other-keys)
  (declare (ignore direct-default-initargs
                   direct-slots
                   direct-superclasses
                   metaclass))
  (apply #'ensure-class-using-class-class class name keys))

(defmethod ensure-class-using-class
    ((class forward-referenced-class)
     name
     &rest keys
     &key
       direct-default-initargs
       direct-slots
       direct-superclasses
       metaclass
     &allow-other-keys)
  (declare (ignore direct-default-initargs
                   direct-slots
                   direct-superclasses
                   metaclass))
  (apply #'ensure-class-using-class-forward-referenced-class class name keys))
