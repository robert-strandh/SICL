(cl:in-package #:sicl-boot-phase-4)

(defun enable-defgeneric (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (import-functions-from-host
     '(endp cons)
     e4)
    (let* ((gf-class-name 'standard-generic-function)
           (gf-class (sicl-genv:find-class gf-class-name e3))
           (method-class-name 'standard-method)
           (method-class (sicl-genv:find-class method-class-name e3))
           (make-instance (sicl-genv:fdefinition 'make-instance e4))
           (method-combination
             (funcall (sicl-genv:fdefinition
                       'sicl-method-combination:find-method-combination e4)
                      'standard '() e4)))
      (setf (sicl-genv:fdefinition 'ensure-generic-function e5)
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
