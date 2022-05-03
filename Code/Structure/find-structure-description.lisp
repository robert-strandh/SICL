(cl:in-package #:sicl-structure)

(let* ((environment (env:global-environment))
       (client (env:client environment))
       (global-environment-function
         (env:fdefinition
          client environment 'env:global-environment))
       (structure-description-function
         (env:fdefinition
          client environment 'env:structure-description))
       (setf-structure-description-function
         (env:fdefinition
          client environment '(setf env:structure-description))))
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
