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
  (sicl-boot:copy-macro-functions e5 e)
  (import-functions-from-host
   '((setf env:function-description)
     env:make-simple-function-description
     (setf env:class-description)
     env:make-class-description)
   e)
  (import-function e5 e 'make-instance))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (ecs sicl-boot:ecs))
      boot
    (change-class ecs 'environment)
    (change-class (env:client ecs) 'client)
    (let ((client (env:client ecs)))
      (setf (env:fdefinition client ecs 'sicl-boot:ast-eval)
            (lambda (ast)
              (sicl-ast-evaluator:eval-ast ast ecs))))
    (pre-fill-environment e5 ecs)
    (define-ensure-generic-function ecs)))
