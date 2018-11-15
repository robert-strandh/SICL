(cl:in-package #:sicl-new-boot-phase-4)

(defun ensure-generic-function-phase-4 (boot)
  (with-accessors ((e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)
                   (e5 sicl-new-boot:e5))
      boot
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


(defun enable-defgeneric-in-e5 (boot)
  (with-accessors ((e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)
                   (e5 sicl-new-boot:e5))
      boot
    (ensure-generic-function-phase-4 boot)
    (import-function-from-host 'sicl-clos:defgeneric-expander e5)
    (load-file "CLOS/defgeneric-defmacro.lisp" e5)))



