(cl:in-package #:sicl-boot-phase-4)

(defmethod sicl-ast-evaluator:translate-ast
    ((client client)
     (ast cleavir-ast:standard-object-p-ast)
     lexical-environment)
  `(let ((object ,(sicl-ast-evaluator:translate-ast
                   client
                   (cleavir-ast:object-ast ast)
                   lexical-environment)))
     (not (or (floatp object)
              (characterp object)
              (typep object 'fixnum)
              (consp object)))))

(defmethod sicl-ast-evaluator:translate-ast
    ((client client)
     (ast cleavir-ast:standard-object-class-of-ast)
     lexical-environment)
  `(funcall ,(lambda (object)
               (cond ((typep object 'sicl-boot::header)
                      (slot-value object 'sicl-boot::%class))
                     ((null object)
                      (env:find-class client (e4 client) 'sicl-boot::host-null))
                     ((symbolp object)
                      (env:find-class client (e4 client) 'sicl-boot::host-symbol))
                     ((stringp object)
                      (env:find-class client (e4 client) 'string))
                     (t
                      (error "Class of ~s asked for in E4" object))))
            ,(sicl-ast-evaluator:translate-ast
              client
              (cleavir-ast:standard-object-ast ast)
              lexical-environment)))

(defmethod sicl-ast-evaluator:translate-ast
    ((client client)
     (ast cleavir-ast:single-float-p-ast)
     lexical-environment)
  `(let ((object ,(sicl-ast-evaluator:translate-ast
                   client
                   (cleavir-ast:object-ast ast)
                   lexical-environment)))
     (floatp object)))
