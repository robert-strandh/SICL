(cl:in-package #:sicl-clos)

(defgeneric ensure-method-combination-template-using-class
    (method-combination-template
     name
     variant-signature-determiner
     effective-method-form-function
     &key documentation client environment))

(defmethod ensure-method-combination-template-using-class
    ((method-combination-template null)
     name
     variant-signature-determiner
     effective-method-form-function
     &key
       documentation
       (client sicl-environment:*client*)
       (environment sicl-environment:*environment*))
  (let ((template
          (make-instance 'clostrophilia:method-combination-template
            :name name
            :documentation documentation
            :variant-signature-determiner variant-signature-determiner
            :effective-method-form-function effective-method-form-function)))
    (setf (sicl-environment:find-method-combination-template
           client environment name)
          template))
  name)
