(cl:in-package #:sicl-type)

(setf (fdefinition 'type-expander)
      (load-time-value
       (let* ((environment (sicl-environment:global-environment))
              (client (sicl-environment:client environment))
              (type-expander
                (fdefinition 'sicl-environment:type-expander)))
         (lambda (type-descriptor)
           (funcall type-expander client environment type-descriptor)))))
