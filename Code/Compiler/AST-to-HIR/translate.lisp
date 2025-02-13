(cl:in-package #:sicl-ast-to-hir)

(defgeneric translate-ast (client ast context))

(defun translate (client ast context)
  (let ((*registers* (make-hash-table :test #'eq)))
    (translate-ast client ast context)))
