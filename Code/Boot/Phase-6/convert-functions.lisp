(cl:in-package #:sicl-boot-phase-6)

(defun wrap-function (function make-instance)
  (let ((wrapper (funcall make-instance 'sicl-clos:simple-function)))
    (setf (sicl-boot:original-function wrapper) function)
    (sicl-host-mop:set-funcallable-instance-function wrapper function)
    wrapper))

(defun convert-functions (e5)
  (let* ((client (env:client e5))
         (make-instance (env:fdefinition client e5 'make-instance)))
    (env:map-defined-functions
     client e5
     (lambda (name function)
       (unless (typep function 'sicl-boot:header)
         (setf (env:fdefinition client e5 name)
               (wrap-function function make-instance)))))))
