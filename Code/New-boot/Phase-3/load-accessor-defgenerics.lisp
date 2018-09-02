(cl:in-package #:sicl-new-boot-phase-3)

(defun ensure-generic-function-phase-3 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e4 sicl-new-boot:e4)) boot
    (let* ((gf-class-name 'standard-generic-function)
           (gf-class (sicl-genv:find-class gf-class-name e2))
           (method-class-name 'standard-method)
           (method-class (sicl-genv:find-class method-class-name e2)))
      (setf (sicl-genv:fdefinition 'ensure-generic-function e4)
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
                                 args)))))))))

(defun activate-generic-function-initialization (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)) boot
    (import-functions-from-host '(set-difference) e3)))

(defun load-accessor-defgeneric (boot)
  (ensure-generic-function-phase-3 boot)
  (activate-generic-function-initialization boot))
