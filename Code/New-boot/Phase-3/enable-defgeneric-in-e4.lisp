(cl:in-package #:sicl-new-boot-phase-3)

(defun ensure-generic-function-phase-3 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)) boot
    (let* ((gf-class-name 'standard-generic-function)
           (method-class-name 'standard-method)
           (method-class (sicl-genv:find-class method-class-name e2))
           (make-instance (sicl-genv:fdefinition 'make-instance e3))
           (method-combination
             (funcall (sicl-genv:fdefinition
                       'sicl-method-combination:find-method-combination e3)
                      'standard '() e3)))
      (setf (sicl-genv:fdefinition 'ensure-generic-function e4)
            (lambda (function-name &rest arguments
                     &key environment
                     &allow-other-keys)
              (let ((args (copy-list arguments)))
                (loop while (remf args :environment))
                (if (sicl-genv:fboundp function-name environment)
                    (sicl-genv:fdefinition function-name environment)
                    (setf (sicl-genv:fdefinition function-name environment)
                          (apply make-instance
                                 gf-class-name
                                 :name function-name
                                 :method-class method-class
                                 :method-combination method-combination
                                 args)))))))))


(defun enable-defgeneric-in-e4 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4))
      boot
    (ensure-generic-function-phase-3 boot)
    (import-function-from-host 'sicl-clos:defgeneric-expander e4)
    (load-file "CLOS/defgeneric-defmacro.lisp" e4)))



