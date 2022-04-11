(cl:in-package #:sicl-boot-condition-system)

(defun import-function (e5 e name)
  (env:import-function (env:client e5) e5 name e))

(defun import-functions (e5 e names)
  (loop for name in names
        do (import-function e5 e name)))

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
  (import-functions
   e5 e
   '(make-instance
     sicl-clos:ensure-class
     ensure-generic-function
     (setf symbol-value)
     sicl-clos::make-class-specializer
     sicl-clos::ensure-method
     boundp
     typep
     sicl-clos:method-function
     simple-condition-format-control
     simple-condition-format-arguments
     type-error-datum
     type-error-expected-type
     cell-error-name
     unbound-slot-instance)))

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
    (define-ast-eval ecs)
    (pre-fill-environment e5 ecs)))
