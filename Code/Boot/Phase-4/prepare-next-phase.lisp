(cl:in-package #:sicl-boot-phase-4)

(defun define-ast-eval (e5)
  (setf (env:fdefinition (env:client e5) e5 'sicl-boot:ast-eval)
        (lambda (ast)
          (let* ((client (env:client e5))
                 (code-object (sicl-compiler:compile-ast client ast)))
            (sicl-compiler:tie-code-object code-object e5)))))

(defun prepare-next-phase (e3 e4 e5)
  (define-ast-eval e5))
