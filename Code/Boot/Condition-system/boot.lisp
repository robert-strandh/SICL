(cl:in-package #:sicl-boot-condition-system)

(defun define-ensure-generic-function (environment)
  (let* ((client (env:client environment))
         (make-instance (env:fdefinition client environment 'make-instance)))
    (setf (env:fdefinition  client environment 'ensure-generic-function)
          (lambda (function-name &key lambda-list &allow-other-keys)
            (funcall make-instance 'standard-generic-function
                     :name function-name
                     :lambda-list lambda-list)))))

(defun import-function (e5 e name)
  (setf (env:fdefinition (env:client e) e name)
        (env:fdefinition (env:client e5) e5 name)))

(defun pre-fill-environment (e5 e)
  (import-function e5 e 'make-instance))

(defun boot (boot)
  (let* ((e5 (sicl-boot:e5 boot))
         (client (make-instance 'client))
         (environment (make-instance 'environment :client client)))
    (pre-fill-environment e5 environment)
    (define-ensure-generic-function environment)))
