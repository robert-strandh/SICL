(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/ensure-class-using-class.html
(defgeneric ensure-class-using-class
    (class
     name
     &rest keys
     &key
       direct-default-initargs
       direct-slots
       direct-superclasses
       metaclass
       &allow-other-keys))
