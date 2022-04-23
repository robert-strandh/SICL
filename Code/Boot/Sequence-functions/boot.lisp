(cl:in-package #:sicl-boot-sequence-functions)

(defun pre-fill-environment (e5 e)
  (sicl-boot:copy-macro-functions e5 e))

(defun define-ast-eval (esf)
  (setf (env:fdefinition (env:client esf) esf 'sicl-boot:ast-eval)
        (lambda (ast)
          (let* ((client (env:client esf))
                 (code-object (sicl-compiler:compile-ast client ast)))
            (sicl-compiler:tie-code-object code-object esf)))))

(defun invoke-with-modified-e5 (e5 esf thunk)
  (let (;; Save the existing function (SETF FDEFINITION) in E5.
        (setf-fdefinition
          (env:fdefinition (env:client e5) e5 '(setf fdefinition))))
    ;; Modify the function (SETF FDEFINITION) in E5 so that it sets
    ;; the definition in ESF instead.
    (setf (env:fdefinition (env:client e5) e5 '(setf fdefinition))
          (lambda (new-function name)
            (setf (env:fdefinition (env:client esf) esf name)
                  new-function)))
    (unwind-protect
         (funcall thunk)
      ;; Restore the saved version of (SETF FDEFINITION) in E5.
      (setf (env:fdefinition (env:client e5) e5 '(setf fdefinition))
            setf-fdefinition))))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (esf (make-instance 'environment :client client)))
      (define-ast-eval esf)
      (pre-fill-environment e5 esf)
      esf)))
