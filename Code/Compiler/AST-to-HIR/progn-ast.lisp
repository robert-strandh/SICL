(cl:in-package #:sicl-ast-to-hir)

(defun translate-implicit-progn (client asts)
  (cond ((null asts)
         (let ((literal-ast (make-instance 'ico:literal-ast :literal 'nil)))
           (translate-ast client literal-ast)))
        ((null (rest asts))
         (translate-ast client (first asts)))
        (t
         (let ((*next-instruction*
                 (translate-implicit-progn client (rest asts)))
               (*values-count* 0))
           (translate-ast client (first asts))))))

(defmethod translate-ast (client (ast ico:progn-ast))
  (translate-implicit-progn client (ico:form-asts ast)))
