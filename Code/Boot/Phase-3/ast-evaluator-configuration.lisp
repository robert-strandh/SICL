(cl:in-package #:sicl-boot-phase-3)

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
               (cond ((null object)
                      (env:find-class client (sicl-boot:environment client)
                                      'null))
                     ((symbolp object)
                      (env:find-class client (sicl-boot:environment client)
                                      'symbol))
                     ((stringp object)
                      (env:find-class client (sicl-boot:environment client)
                                      'string))
                     (t
                      (class-of object))))
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
