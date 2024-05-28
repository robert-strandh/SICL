(cl:in-package #:sicl-clos)

(defun clostrophilia::ensure-method-combination-template
    (name
     variant-signature-determiner
     effective-method-form-function
     &key documentation)
  (let ((template (sicl-environment:find-method-combination-template
                   sicl-environment:*client*
                   sicl-environment:*environment*
                   name)))
    (^ensure-method-combination-template-using-class
     template
     name
     variant-signature-determiner
     effective-method-form-function
     :documentation documentation
     :client sicl-environment:*client*
     :environment sicl-environment:*environment*)))
