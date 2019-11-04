(cl:in-package #:sicl-boot-phase-6)

(defun enable-defgeneric (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e6 sicl-boot:e6)
                   (e7 sicl-boot:e7))
      boot
    (let* ((gf-class-name 'standard-generic-function)
           (gf-class (sicl-genv:find-class gf-class-name e5))
           (method-class-name 'standard-method)
           (method-class (sicl-genv:find-class method-class-name e5))
           (make-instance (sicl-genv:fdefinition 'make-instance e6))
           (method-combination
             (funcall (sicl-genv:fdefinition
                       'sicl-method-combination:find-method-combination e6)
                      'standard '() e6)))
      (setf (sicl-genv:fdefinition 'ensure-generic-function e7)
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
                                 args)))))))))
