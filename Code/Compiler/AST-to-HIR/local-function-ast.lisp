(cl:in-package #:sicl-ast-to-hir)

;;; A "register lambda list" is like an ordinary lambda list, but with
;;; no INIT-FORMs and where variables have been replaced by registers,
;;; and there is not &AUX section.  Specifically, each reguired
;;; parameter is a register.  When &OPTIONAL is present, each optional
;;; parameter is a list of two registers.  The first register
;;; corresponds to the parameter itself, and the second register
;;; corresponds to a SUPPLIED-P parameter.  When &REST is present, the
;;; rest parameter is a register.  When &KEY is present, each key
;;; parameter is a list of two elements.  The first element is again a
;;; list of two element, they keyword to be used to recognize the
;;; argument, and a register corresponding to the parameter itself.
;;; The second element is a register corresponding to the SUPPLIED-P
;;; parameter.

(defgeneric register-parameter-from-parameter-ast (parameter-ast))

(defmethod register-parameter-from-parameter-ast
    ((ast ico:required-parameter-ast))
  (find-register (ico:name-ast ast)))

(defmethod register-parameter-from-parameter-ast
    ((ast ico:optional-parameter-ast))
  (list (find-register (ico:name-ast ast))
        (find-register (ico:supplied-p-parameter-ast ast))))

(defmethod register-parameter-from-parameter-ast
    ((ast ico:rest-parameter-ast))
  (find-register (ico:name-ast ast)))

(defmethod register-parameter-from-parameter-ast
    ((ast ico:key-parameter-ast))
  (let* ((name-ast (ico:name-ast ast))
         (keyword-ast (ico:keyword-ast ast))
         (keyword-name
           (if (null keyword-ast)
               (intern (symbol-name (ico:name name-ast)) "KEYWORD")
               (ico:literal keyword-ast))))
    (list (list keyword-name (find-register name-ast))
          (find-register (ico:supplied-p-parameter-ast ast)))))

(defgeneric register-section-from-section-ast (section-ast))

(defmethod register-section-from-section-ast ((ast null))
  '())
  
(defmethod register-section-from-section-ast
    ((ast ico:required-section-ast))
  (loop for parameter-ast in (ico:parameter-asts ast)
        collect
        (register-parameter-from-parameter-ast parameter-ast)))
  
(defmethod register-section-from-section-ast
    ((ast ico:optional-section-ast))
  (cons '&optional
        (loop for parameter-ast in (ico:parameter-asts ast)
              collect (register-parameter-from-parameter-ast
                       parameter-ast))))
  
(defmethod register-section-from-section-ast
    ((ast ico:key-section-ast))
  (append (cons '&key
                (loop for parameter-ast in (ico:parameter-asts ast)
                      collect (register-parameter-from-parameter-ast
                               parameter-ast)))
          (if (null (ico:allow-other-keys-ast ast))
              '()
              (list '&allow-other-keys))))

(defmethod register-section-from-section-ast
    ((ast ico:rest-section-ast))
  (list '&rest
        (register-parameter-from-parameter-ast (ico:parameter-ast ast))))

(defun register-lambda-list-from-lambda-list-ast (lambda-list-ast)
  (loop for accessor in (list #'ico:required-section-ast
                              #'ico:optional-section-ast
                              #'ico:rest-section-ast
                              #'ico:key-section-ast)
        append (register-section-from-section-ast
                (funcall accessor lambda-list-ast))))

(defmethod translate-ast (client (ast ico:local-function-ast))
  (let* ((lambda-list-ast (ico:lambda-list-ast ast))
         (variable-definition-asts
           (iat:extract-variable-asts-in-lambda-list lambda-list-ast))
         (registers
           (loop for variable-definition-ast in variable-definition-asts
                 collect (make-instance 'hir:single-value-register))))
    (loop for variable-definition-ast in variable-definition-asts
          for register in registers
          do (setf (find-register variable-definition-ast) register))
    (let* ((*registers* (make-hash-table :test #'eq))
           (*target-register*
             (make-instance 'hir:multiple-value-register))
           (*dynamic-environment-register*
             (make-instance 'hir:single-value-register))
           (*static-environment-register*
             (make-instance 'hir:single-value-register))
           (*next-instruction*
             (make-instance 'hir:return-instruction
               :inputs (list *target-register*)))
           (*unwind-instructions-to-fix-up* '())
           (body-instruction
             (translate-implicit-progn client (ico:form-asts ast))))
      (fix-up-unwind-instructions)
      (make-instance 'hir:parse-arguments-instruction
        :inputs '()
        :outputs registers
        :successors (list body-instruction)))))
