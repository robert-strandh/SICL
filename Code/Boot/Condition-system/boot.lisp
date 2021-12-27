(cl:in-package #:sicl-boot-condition-system)

(defun define-ensure-generic-function (environment)
  (let* ((client (env:client environment))
         (make-instance (env:fdefinition client environment 'make-instance)))
    (setf (env:fdefinition  client environment 'ensure-generic-function)
          (lambda (function-name &key lambda-list &allow-other-keys)
            (funcall make-instance 'standard-generic-function
                     :name function-name
                     :lambda-list lambda-list)))))

(defun boot (boot)
  (let* ((client (make-instance 'client))
         (environment (make-instance 'environment :client client)))
    (define-ensure-generic-function environment)))
