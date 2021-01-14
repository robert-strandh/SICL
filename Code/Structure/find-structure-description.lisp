(cl:in-package #:sicl-structure)

(let* ((environment (sicl-environment:global-environment))
       (client (sicl-environment:client environment))
       (global-environment-function
         (sicl-environment:fdefinition
          client environment 'sicl-environment:global-environment))
       (structure-description-function
         (sicl-environment:fdefinition
          client environment 'sicl-environment:structure-description))
       (setf-structure-description-function
         (sicl-environment:fdefinition
          client environment '(setf sicl-environment:structure-description))))
  (defun find-structure-description (name &optional (errorp t) environment)
    (let* ((global-environment
             (funcall global-environment-function environment))
           (description
             (funcall structure-description-function
                      name global-environment)))
      (if (and (null description) errorp)
          (error 'undefined-structure-description :name name)
          description)))

  (defun (setf find-structure-description)
      (new-value name &optional (errorp t) environment)
    (declare (ignore errorp))
    (let ((global-environment
            (funcall global-environment-function environment)))
      (funcall setf-structure-description-function
               new-value name global-environment-function))))
