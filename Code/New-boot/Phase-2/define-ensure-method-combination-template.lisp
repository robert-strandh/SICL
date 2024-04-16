(cl:in-package #:sicl-new-boot-phase-2)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun define-ensure-method-combination-template (client e1 e2)
  (setf (clo:fdefinition
         client e2 @clostrophilia:ensure-method-combination-template)
        (lambda (name
                 variant-signature-determiner
                 effective-method-form-function
                 &key documentation)
          (let ((template
                  (sicl-environment:find-method-combination-template
                    client e2 name)))
            (when (null template)
              (let* ((class (clo:find-class
                             client e1
                             @clostrophilia:method-combination-template t)))
                (setf template
                      (make-instance class
                        :name name
                        :documentation documentation
                        :variant-signature-determiner
                        variant-signature-determiner
                        :effective-method-form-function
                        effective-method-form-function))
                (setf (sicl-environment:find-method-combination-template
                       client e2 name)
                      template)))
            name))))
