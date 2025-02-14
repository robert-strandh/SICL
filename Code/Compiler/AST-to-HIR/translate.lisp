(cl:in-package #:sicl-ast-to-hir)

(defgeneric translate-ast (client ast))

(defun translate (client ast)
  (let* ((*registers* (make-hash-table :test #'eq))
         (*target-register*
           (make-instance 'hir:multiple-value-register))
         (*values-count* :all)
         (*next-instruction*
           (make-instance 'hir:return-instruction
             :inputs (list *target-register*))))
    (translate-ast client ast)))
