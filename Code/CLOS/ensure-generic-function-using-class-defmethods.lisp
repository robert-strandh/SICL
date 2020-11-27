(cl:in-package #:sicl-clos)

(defmethod ensure-generic-function-using-class
    ((generic-function null)
     function-name
     &rest
       all-keyword-arguments
     &key
       generic-function-class
       method-class
     &allow-other-keys)
  (declare (ignore generic-function-class method-class))
  (setf (fdefinition function-name)
        (apply #'ensure-generic-function-using-class-null
               generic-function
               function-name
               all-keyword-arguments)))

(defmethod ensure-generic-function-using-class
    ((generic-function generic-function)
     function-name
     &rest
       all-keyword-arguments
     &key
       generic-function-class
       method-class
     &allow-other-keys)
  (declare (ignore generic-function-class method-class))
  (apply #'ensure-generic-function-using-class-generic-function
         generic-function
         function-name
         all-keyword-arguments))
