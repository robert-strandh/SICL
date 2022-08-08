(cl:in-package #:sicl-boot-condition-system)

(defun pre-fill-environment (e5 e)
  (sicl-boot:copy-macro-functions e5 e))

(defun define-ast-eval (ecs)
  (setf (env:fdefinition (env:client ecs) ecs 'sicl-boot:ast-eval)
        (lambda (client ast)
          (sicl-boot-compile-and-tie:compile-and-tie
           client ecs ast))))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (ecs sicl-boot:ecs))
      boot
    (change-class ecs 'environment :base e5)
    (change-class (env:client ecs) 'client)
    (define-ast-eval ecs)
    (pre-fill-environment e5 ecs)))
