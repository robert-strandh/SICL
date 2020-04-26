(cl:in-package #:sicl-boot-phase-6)

(defun enable-defgeneric (ea eb ec)
  (let* ((gf-class-name 'standard-generic-function)
         (gf-class (sicl-genv:find-class gf-class-name ea))
         (method-class-name 'standard-method)
         (method-class (sicl-genv:find-class method-class-name ea))
         (make-instance (sicl-genv:fdefinition 'make-instance eb))
         (method-combination
           (funcall (sicl-genv:fdefinition
                     'sicl-method-combination:find-method-combination eb)
                    'standard '() eb)))
    (setf (sicl-genv:fdefinition 'ensure-generic-function ec)
          (lambda (function-name &rest arguments
                   &key environment
                   &allow-other-keys)
            (let ((args (copy-list arguments)))
              (loop while (remf args :environment))
              (if (sicl-genv:fboundp function-name environment)
                  (sicl-genv:fdefinition function-name environment)
                  (setf (sicl-genv:fdefinition function-name environment)
                        (apply make-instance
                               gf-class
                               :name function-name
                               :method-class method-class
                               :method-combination method-combination
                               args))))))))
