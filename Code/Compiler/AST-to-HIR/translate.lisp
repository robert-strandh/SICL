(cl:in-package #:sicl-ast-to-hir)

(defgeneric translate-ast (client ast context))

(defun translate (client ast)
  (let* ((*registers* (make-hash-table :test #'eq))
         (register
           (make-instance 'hir:multiple-value-register))
         (return-instruction
           (make-instance 'hir:return-instruction
             :inputs (list register)))
         (context
           (make-context :all return-instruction register)))
    (translate-ast client ast context)))
