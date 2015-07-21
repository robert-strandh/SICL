(cl:in-package #:sicl-clos)

(defclass slot-definition (metaobject)
  ((%name 
    :initarg :name
    :reader slot-definition-name)
   (%allocation 
    :initarg :allocation
    :initform :instance
    :reader slot-definition-allocation)
   (%type 
    :initarg :type
    :initform t
    :reader slot-definition-type)
   (%initargs 
    :initform '()
    :initarg :initargs 
    :reader slot-definition-initargs)
   (%initform 
    :initarg :initform 
    :initform nil
    :reader slot-definition-initform)
   (%initfunction 
    :initform nil
    :initarg :initfunction
    :reader slot-definition-initfunction)))
