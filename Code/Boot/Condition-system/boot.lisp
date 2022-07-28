(cl:in-package #:sicl-boot-condition-system)

(defun pre-fill-environment (e5 e)
  (sicl-boot:copy-macro-functions e5 e))

(defun define-ast-eval (ecs)
  (setf (env:fdefinition (env:client ecs) ecs 'sicl-boot:ast-eval)
        (lambda (ast)
          (let ((client (env:client ecs)))
            (multiple-value-bind (code-object hir-thunks)
                (sicl-compiler:compile-ast client ast)
              (sicl-compiler:tie-code-object
               client ecs code-object hir-thunks))))))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (ecs sicl-boot:ecs))
      boot
    (change-class ecs 'environment :base e5)
    (change-class (env:client ecs) 'client)
    (define-ast-eval ecs)
    (pre-fill-environment e5 ecs)))
