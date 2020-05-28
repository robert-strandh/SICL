(cl:in-package #:sicl-clos)

(defun find-or-create-generic-function (function-name lambda-list)
  (if (fboundp function-name)
      (fdefinition function-name)
      (let* ((environment (sicl-genv:global-environment))
             (generic-function-class (find-class 'standard-generic-function))
             (prototype (sicl-clos:class-prototype generic-function-class))
             (method-combination
               (sicl-clos:find-method-combination prototype 'standard '())))
        (ensure-generic-function
         function-name
         :lambda-list lambda-list
         :generic-function-class generic-function-class
         :method-combination method-combination
         :environment environment))))
