(cl:in-package #:sicl-package)

(symbol-macrolet ((client sicl-client:*client*))
  (let* ((environment (env:global-environment))
         (find-package-function
           (env:fdefinition
            client environment 'env:find-package))
         (setf-find-package-function
           (env:fdefinition
            client environment '(setf env:find-package))))
    (defun find-package (name)
      (if (typep name 'package)
          name
          (funcall find-package-function client environment name)))
    (defun (setf %find-package) (package name)
      (funcall setf-find-package-function
               package client environment name))))
