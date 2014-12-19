(cl:in-package #:sicl-clos)

(defclass generic-function (metaobject funcallable-standard-object)
  (;; While there is a function named (SETF GENERIC-FUNCTION-NAME), it
   ;; is not a writer function in that it works by calling
   ;; REINITIALIZE-INSTANCE.
   (%name 
    :initform nil
    :initarg :name
    :reader generic-function-name)
   (%lambda-list 
    :initarg :lambda-list
    :reader generic-function-lambda-list)
   (%documentation 
    :initarg :documentation
    :initform nil
    :reader gf-documentation)
   (%dependents
    :initform '()
    :accessor dependents))
  (:metaclass funcallable-standard-class))
