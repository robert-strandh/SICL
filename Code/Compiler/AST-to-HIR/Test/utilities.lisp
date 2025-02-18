(cl:in-package #:sicl-ast-to-hir-test)

(defun import-host-function (client name global-environment)
  (setf (clostrum:fdefinition client global-environment name)
        (fdefinition name)))

(defmacro with-default-parameters
    ((client-variable environment-variable global-environment-variable)
     &body body)
  `(let* ((,client-variable (make-instance 'client))
          (,environment-variable (cb:create-environment))
          (,global-environment-variable
            (trucler:global-environment
             ,client-variable ,environment-variable)))
     (declare (ignorable ,global-environment-variable))
     (import-host-function
      ,client-variable 'null ,global-environment-variable)
     (import-host-function
      ,client-variable 'list ,global-environment-variable)
     (import-host-function
      ,client-variable 'first ,global-environment-variable)
     (import-host-function
      ,client-variable 'rest ,global-environment-variable)
     (import-host-function
      ,client-variable 'error ,global-environment-variable)
     (import-host-function
      ,client-variable '+ ,global-environment-variable)
     (import-host-function
      ,client-variable '1+ ,global-environment-variable)
     (import-host-function
      ,client-variable '> ,global-environment-variable)
     (import-host-function
      ,client-variable 'floor ,global-environment-variable)
     (import-host-function
      ,client-variable 'funcall ,global-environment-variable)
     (import-host-function
      ,client-variable 'values ,global-environment-variable)
     ,@body))

(defmacro iss (form1 form2)
  `(is #'equal
       (multiple-value-list ,form1)
       (multiple-value-list ,form2)))

(defun eval-expression (client expression environment)
  (cb:eval-expression client expression environment))
