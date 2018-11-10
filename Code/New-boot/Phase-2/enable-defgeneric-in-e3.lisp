(cl:in-package #:sicl-new-boot-phase-2)

(defun ensure-generic-function-phase-2 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3))
      boot
    (let* ((class-env (sicl-new-boot:e1 boot))
           (gf-class-name 'standard-generic-function)
           (gf-class (sicl-genv:find-class gf-class-name class-env))
           (method-class-name 'standard-method)
           (method-class (sicl-genv:find-class method-class-name class-env))
           (target-env e3)
           (method-combination
             (funcall (sicl-genv:fdefinition
                       'sicl-method-combination:find-method-combination
                       e2)
                      'standard '() e2)))
      (setf (sicl-genv:fdefinition 'ensure-generic-function target-env)
            (lambda (function-name &rest arguments
                     &key environment
                     &allow-other-keys)
              (let ((args (copy-list arguments)))
                (loop while (remf args :environment))
                (if (sicl-genv:fboundp function-name environment)
                    (sicl-genv:fdefinition function-name environment)
                    (setf (sicl-genv:fdefinition function-name environment)
                          (apply #'make-instance gf-class
                                 :name function-name
                                 :method-class method-class
                                 :method-combination method-combination
                                 args)))))))))

(defun enable-defgeneric-in-e3 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3))
      boot
    (ensure-generic-function-phase-2 boot)
    (import-function-from-host 'sicl-clos:defgeneric-expander e3)
    (load-file "CLOS/defgeneric-defmacro.lisp" e3)))
