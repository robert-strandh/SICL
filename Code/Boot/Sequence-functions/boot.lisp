(cl:in-package #:sicl-boot-sequence-functions)

(defun pre-fill-environment (e5 e)
  (sicl-boot:copy-macro-functions e5 e))

(defun define-ast-eval (esf)
  (setf (env:fdefinition (env:client esf) esf 'sicl-boot:ast-eval)
        (lambda (ast)
          (let* ((client (env:client esf))
                 (code-object (sicl-compiler:compile-ast client ast)))
            (sicl-compiler:tie-code-object code-object esf)))))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (esf (make-instance 'environment :client client)))
      (define-ast-eval esf)
      (pre-fill-environment e5 esf))))
