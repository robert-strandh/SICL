(cl:in-package #:sicl-clos)

(defgeneric ensure-method-using-generic-function
    (generic-function
     &key
       unspecialized-lambda-list
       qualifiers
       specializer-designators
       documentation
       function))

(defmethod ensure-method-using-generic-function
    ((generic-function standard-generic-function)
     &key
       unspecialized-lambda-list
       qualifiers
       specializer-designators
       documentation
       function)
  (let* ((specializers
           (loop for specializer-designator in specializer-designators
                 collect (cond ((consp specializer-designator)
                                (make-instance 'clostrophilia:eql-specializer
                                  :object (second specializer-designator)))
                               ((symbolp specializer-designator)
                                (find-class specializer-designator))
                               (t specializer-designator))))
         (method-class
           (clostrophilia:generic-function-method-class generic-function))
         (method
           (make-instance method-class
             :lambda-list unspecialized-lambda-list
             :qualifiers qualifiers
             :specializers specializers
             :documentation documentation
             :function function)))
    (clostrophilia:add-method generic-function method)))
    
                               


