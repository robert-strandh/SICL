(cl:in-package #:sicl-boot-condition-system)

(defun import-function (e5 e name)
  (setf (env:fdefinition (env:client e) e name)
        (env:fdefinition (env:client e5) e5 name)))

(defun pre-fill-environment (e5 e)
  (sicl-boot:copy-macro-functions e5 e)
  (import-functions-from-host
   '((setf env:function-description)
     env:make-simple-function-description
     (setf env:variable-description)
     env:make-constant-variable-description
     env:make-special-variable-description
     (setf env:class-description)
     env:make-class-description)
   e)
  (import-function e5 e 'make-instance)
  (import-function e5 e 'sicl-clos:ensure-class)
  (import-function e5 e 'ensure-generic-function)
  (import-function e5 e '(setf symbol-value))
  (import-function e5 e 'sicl-clos::make-class-specializer)
  (import-function e5 e 'sicl-clos::ensure-method)
  (import-function e5 e 'boundp))

(defun define-ast-eval (ecs)
  (setf (env:fdefinition (env:client ecs) ecs 'sicl-boot:ast-eval)
        (lambda (ast)
          (let* ((client (env:client ecs))
                 (code-object (sicl-compiler:compile-ast client ast)))
            (sicl-compiler:tie-code-object code-object ecs)))))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (ecs sicl-boot:ecs))
      boot
    (change-class ecs 'environment)
    (change-class (env:client ecs) 'client)
    (pre-fill-environment e5 ecs)))
