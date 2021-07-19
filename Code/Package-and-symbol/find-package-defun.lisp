(cl:in-package #:sicl-package)

(let* ((environment (sicl-environment:global-environment))
       (client (sicl-environment:client environment))
       (find-package-function
         (sicl-environment:fdefinition
          client environment 'sicl-environment:find-package))
       (setf-find-package-function
         (sicl-environment:fdefinition
          client environment '(setf sicl-environment:find-package))))
  (defun find-package (name)
    (if (typep name 'package)
        name
        (funcall find-package-function client environment name)))
  (defun (setf %find-package) (package name)
    (funcall setf-find-package-function
             package client environment name)))
