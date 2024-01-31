(cl:in-package #:sicl-new-boot-phase-2)

(defun define-ensure-method-combination-template (client e1 e2)
  (let ((symbol
          (sb:intern-parcl-symbol
           client
           "CLOSTROPHILIA"
           "ENSURE-METHOD-COMBINATION-TEMPLATE")))
    (setf (clo:fdefinition client e2 symbol)
          (lambda (name
                   &key
                     documentation
                     variant-signature-determiner
                     effective-method-form-function)
            (let ((template
                    (sicl-environment:find-method-combination-template
                     name e2)))
              (when (null template)
                (let* ((symbol
                         (sb:intern-parcl-symbol
                          client
                          "SICL-METHOD-COMBINATION"
                          "METHOD-COMBINATION-TEMPLATE"))
                       (class (clo:find-class client e1 symbol t)))
                  (setf template
                        (make-instance class
                          :name name
                          :documentation documentation
                          :variant-signature-determiner
                          variant-signature-determiner
                          :effective-method-form-function
                          effective-method-form-function))
                  (setf (sicl-environment:find-method-combination-template
                         name e2)
                        template))))
            name))))
